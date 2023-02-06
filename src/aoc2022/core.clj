(ns aoc2022.core
  "Douglas P. Fields, Jr.'s Advent of Code 2022 solutions in Clojure.
   Copyright 2022 Douglas P. Fields, Jr. All Rights Reserved.
   symbolics@lisp.engineer"
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; IntelliJ hotkeys on Windows with Cursive:
;; Alt-Shift-P - Send form to REPL
;; Control-\ - Jump to REPL input
;; Alt-Shift-M - "Sync files in REPL"

;; Day 1 - https://adventofcode.com/2022/day/1

(def day1-input
  "Day 1 input file parsed into lines"
  (str/split-lines (slurp "resources/day1-input.txt")))

(def d1-elves
  "Turns the day1-input into sums for each elf."
  (letfn [(f [acc val]
            "If val is non-blank, treat it as a string'd long and add it to the last
             item in the acc vector. If it is blank, we're done with the previous value
             so we should start a new one (at 0)."
            (if (str/blank? val)
              ;; Done with the current elf, add a new elf
              (conj acc 0)
              ;; Add the current number to the last one
              (update acc (dec (count acc)) #(+ %1 (Long/parseLong val)))))]
    (reduce f [0] day1-input)))

(def d1-q1
  "Answer to question one, elf carrying the most"
  (apply max d1-elves))

(def d1-q2
  "D1Q2 - total carried by top three elves"
  (reduce + (take-last 3 (sort d1-elves))))

;; --------------------------------------------------------------------------
;; Day 2

(def d2-input-raw
  "Raw text file input split into lines"
  (str/split-lines (slurp "resources/day2-input.txt")))

;; Let's parse this from "1 2" strings to [:rock :paper] vectors
(defn parse-d2-input-q1
  [s]
  (letfn [(char-to-rps [c]
            (case c
              (\A \X) :rock
              (\B \Y) :paper
              (\C \Z) :scissors
              (throw (ex-info "Invalid rps character" {:input c}))))]
    (let [opponent (first s)
          us (last s)]
      [(char-to-rps opponent) (char-to-rps us)])))

(def d2-input-q1
  "Input split into RPS vectors of [opponent us]."
  (map parse-d2-input-q1 d2-input-raw))

;; The winner of the whole tournament is the player with the highest score.
;; Your total score is the sum of your scores for each round.
;; The score for a single round is the score for the shape you selected
;; (1 for Rock, 2 for Paper, and 3 for Scissors)
;; plus the score for the outcome of the round
;; (0 if you lost, 3 if the round was a draw, and 6 if you won).

(defn shape-score
  "Gets the score for a given shape.
   Note, we could do this with a map lookup too, but I imagine this is faster."
  [sh]
  (case sh
    :rock 1
    :paper 2
    :scissors 3
    (throw (ex-info "Unknown rps shape" {:input sh}))))

(defn rps-outcome
  "What the outcome is for an RPS event. No error checking.
   Input is [opponent us]."
  [[opponent us]]
  (cond
    (= opponent us) :draw
    (= opponent :rock) (if (= us :paper) :win :lose)
    (= opponent :paper) (if (= us :scissors) :win :lose)
    :else ;; :scissors
    (if (= us :rock) :win :lose)))

(defn rps-score
  "Gets the score for a given outcome for us"
  [outcome]
  (case outcome
    :win 6
    :draw 3
    :lose 0
    (throw (ex-info "Invalid rps outcome" {:outcome outcome}))))

(defn round-score
  "Gets the score for one round. Input is: [opponent us]."
  [[_ us :as round]]
  (+ (shape-score us) (rps-score (rps-outcome round))))

(def d2-q1
  "The total score from all rounds."
  (reduce + 0 (map round-score d2-input-q1)))
;; 12276

;; QUESTION 2 ----------------------
;; Anyway, the second column says how the round needs to end:
;; X means you need to lose,
;; Y means you need to end the round in a draw, and
;; Z means you need to win.

;; Let's parse this from "1 2" strings to [:rock :paper] vectors
(defn parse-d2-input-q2
  "Returns [rps-shape desired-outcome]"
  [s]
  (letfn [(char-to-rps [c]
            (case c
              \A :rock
              \B :paper
              \C :scissors
              \X :lose
              \Y :draw
              \Z :win
              (throw (ex-info "Invalid rps-q2 character" {:input c}))))]
    (let [opponent (first s)
          desired-outcome (last s)]
      [(char-to-rps opponent) (char-to-rps desired-outcome)])))

(def d2-input-q2
  "Input split into RPS vectors of [opponent desired-outcome]."
  (map parse-d2-input-q2 d2-input-raw))

(defn shape-for-outcome
  "Returns the [opponent us] to make the
  [opponent desired-outcome] true."
  [[opponent desired-outcome]]
  [opponent
   (case desired-outcome
     :draw opponent
     :win  (case opponent :rock :paper, :paper :scissors, :scissors :rock, :invalid)
     :lose (case opponent :rock :scissors, :paper :rock, :scissors :paper, :invalid))])

(def d2-input-q2-rps
  "Turns the input into a series of turns for RPS game."
  (map shape-for-outcome d2-input-q2))

(def d2-q2
  "The total score from all rounds for Q2."
  (reduce + 0 (map round-score d2-input-q2-rps)))
;; 9975


;; DAY 3 ----------------------------------------------------------------------
;; https://adventofcode.com/2022/day/3

(def d3-input-raw
  "Day 3 input split into raw lines"
  (str/split-lines (slurp "resources/day3-input.txt")))

(def d3-input
  "The day 3 input split into a seq of rucksacks,
   each with two compartments, each of which is
   a vector of its contents (in order). Each item is a character.
   Example: [ [[a b c] [c D E]]
              [[e F G H] [H i j K]] ]
   (It may not be vectors.)
   The order doesn't seem to matter for now.
   The rucksack has two equal compartments."
  (letfn [(make-sack [s]
            (let [chars (mapv char s)
                  cnt (count chars)
                  hlf (/ cnt 2)]
              [(take hlf chars) (drop hlf chars)]))]
    (map make-sack d3-input-raw)))

(defn find-dupe
  "Finds the one and only one duplicate in two seqs.
   (nil if no duplicates; one random duplicate if multiple dups.)"
  [s1 s2]
  (let [set1 (set s1)
        set2 (set s2)]
    (first (set/intersection set1 set2))))

(defn item-priority
  "Lowercase item types a through z have priorities 1 through 26.
   Uppercase item types A through Z have priorities 27 through 52."
  [item]
  (if (Character/isLowerCase ^char item)
    (inc (- (Character/getNumericValue ^char item) (Character/getNumericValue \a)))
    (+ 27 (- (Character/getNumericValue ^char item) (Character/getNumericValue \A)))))

(def d3-q1
  "Answer to day 3 question 1"
  (reduce +
    (map item-priority
      (map (partial apply find-dupe) d3-input))))
;; 7742

;; Day 3 question 2 --------

;; Group input into 3
;; combine each of the 3 rucksack into a single set
;; see what is the same in all 3
;; i.e.: (apply set/intersection (map (comp set flatten) (take 3 d3-input)))
;; for a single example

(def d3-group
  "Groups of elves with rucksacks;
   three elves at a time.
   This gives us a structure like this:
   (
    ( ;; Group 1
     [ ;; Elf Rucksack 1
       (a b c) ;; compartment 1
       (d e c) ;; compartment 2
     ]
     [ ... ] ;; elf rucksack 2
     [ ... ] ;; elf rucksack 3
   ) ;; End of Group 1
   "
  (partition 3 d3-input))

(def d3-group-sets
  "Groups of elves with rucksacks all combined into a set of what's in them;
   three elves at a time.
   This gives us a structure like this:
   (
    ( ;; Group 1
     #{ ;; Elf Rucksack 1
      }
      ...
    ) ;; end of group 1
   ) ;; all elves/rucksacks
   "
  (partition 3 (map (comp set flatten) d3-input)))

(def d3-badges
  "The badge is the unique item in each group of 3 elves's rucksacks."
  (map #(first (apply set/intersection %)) d3-group-sets))

;; Note: Up to here there has been no error checking of these cases:
;; 1. Any last group of less than 3 elves: (some #(not= 3 %) (map count d3-groups)) is nil, so good
;; 2. Any group of 3 elves with no badge: (some nil? d3-badges) is nil, so good
;; 3. That there are several groups

(def d3-q2
  "And add the priorities of the badges"
  (reduce + 0 (map item-priority d3-badges)))
;; 2276


;; Day 4 -----------------------------------------------------------------------

;; This is sad. I built a whole interval arithmetic library in Java decades ago
;; for Puresend. It is probably still in the Puresend code base.

;; We will represent a range as [low high] inclusive.
;; Input is a bunch of lines L-H,L-H.
;; We want to know how many of those lines have one range entirely contained in the other.
;; We will assume L <= H but won't check.

(def d4-input-raw
  "Day 4 input split into raw lines"
  (str/split-lines (slurp "resources/day4-input.txt")))

(defn split-last
  "Like str/split but takes the input string to
   split as the last arg."
  [pattern limit string]
  (str/split string pattern limit))

(def d4-input
  "Day 4 input parsed into
   [ [[L H] [L H]]
     [[L H] [L H]] ... ]
   NO ERROR CHECKING"
  (let [;; Parse into lists of pairs of strings L-H
        half-parsed (map (partial split-last #"," 2) d4-input-raw)
        ;; Parse into lists of pairs of [L H]
        mostly-parsed (mapv (partial mapv (partial split-last #"-" 2)) half-parsed)]
    ;; Now we have to parse every value that is a string. We can touch
    ;; everything with clojure.walk/postwalk
    ;; (See: https://clojuredocs.org/clojure.walk/postwalk )
    (clojure.walk/postwalk
      #(if (string? %) (Long/parseLong %) %)
      mostly-parsed)))

(defn r-contained?
  "Determines if r1 is entirely contained in r2.
   r1 and r2 are [L H] pairs.
   NO ERROR CHECKING"
  [[l1 h1] [l2 h2]]
  (and (>= l1 l2)
       (<= h1 h2)))

(r-contained? [1 2] [3 4])
(r-contained? [1 3] [3 4])
(r-contained? [3 3] [3 4])
(r-contained? [3 4] [3 4])
(r-contained? [3 5] [3 4])
(r-contained? [1 5] [3 4])
(r-contained? [4 5] [3 5])
(r-contained? [4 4] [3 5])

(defn either-contained?
  "Checks if either range is fully contained in the other.
   NO ERROR CHECKING."
  [r1 r2]
  (or (r-contained? r1 r2) (r-contained? r2 r1)))

(either-contained? [4 4] [3 5])
(either-contained? [3 5] [4 4])
(either-contained? [3 5] [2 4])
(either-contained? [3 5] [5 4])
(either-contained? [3 5] [3 5])
(either-contained? [3 5] [3 6])
(either-contained? [3 7] [3 6])
(either-contained? [4 7] [3 6])

(def d4-q1
  "How many pairs have one fully contained in the other?"
  ;; fci = fully contained input, a (t f t f t f) seq of true/false
  (let [fci (map (partial apply either-contained?) d4-input)]
    ;; Now let's count the trues
    (count (filter identity fci))))
;; 657 (of 1000) - super inefficient LOL

;; D4 Part 2 -------------

(defn r-overlap?
  "Determines if two [L H] ranges have any overlap.
   Both ranges are inclusive."
  ;; A silly but easy way to do this would be to make integer sets of
  ;; both ranges and then do a set intersection. Since it seems all the
  ;; numbers in this AoC are 1-99 that should work fine if slowly. :)
  ;; ---
  ;; Assuming l1 <= l2 - these are the possible situations
  ;; L2 = L1
  ;; L1 ---------
  ;; L2 ---                     Contained within
  ;; L2 ---------               Complete overlap
  ;; L2 -----------------       Extends beyond
  ;; L2 > L1 && L2 <= H1
  ;; L1 ---------
  ;; L2   ----                  Contained within
  ;; L2   -------               Overlap to end
  ;; L2   ---------------       Extends beyond
  ;; L2 > H1
  ;; L1 ---------
  ;; L2          -----          Starts beyond
  ;; L2             -------     Starts way beyond
  [[l1 h1 :as r1] [l2 h2 :as r2]]
  ;; Ensure that we only check the case where l1 <= l2
  (if (< l2 l1)
    (recur r2 r1)
    ;; Now check cases
    ;; ASSERT l1 <= l2
    ;; There is overlap if:
    ;; The higher low is lower than the lower low's high
    (<= l2 h1)))

(r-overlap? [1 2] [3 4])
(r-overlap? [3 4] [1 2])
(r-overlap? [1 10] [11 20])
(r-overlap? [1 11] [11 20])
(r-overlap? [1 30] [11 20])
(r-overlap? [11 20] [1 10])
(r-overlap? [11 20] [1 11])
(r-overlap? [11 20] [1 30])
(r-overlap? [2 2] [3 3])
(r-overlap? [3 3] [2 3])
(r-overlap? [3 3] [3 4])
(r-overlap? [4 5] [3 4])

(def d4-q2
  "How many pairs have overlaps?"
  ;; fci = fully contained input, a (t f t f t f) seq of true/false
  (let [fci (map (partial apply r-overlap?) d4-input)]
    ;; Now let's count the trues
    (count (filter identity fci))))
;; 938

;; Day 5 -----------------------------------------------------------

;; (This was done upstate and obviously not checked in yet.)

;; Day 6 ------------------------------------------------------------
;; https://adventofcode.com/2022/day/6

(def d6-input-raw
  "Day 6 input as a single long raw line"
  (str/trim (slurp "resources/day6-input.txt")))

;; Detect start-of-packet marker: four characters that are all different

;; Problem 1:
;; Identify the first position where the four most recently received
;; characers are all different. Report # of chars from beginning of
;; buffer to the end of the first such four-character marker.
;;
;; mjqjpqmgbljsphdztnvjfqwrcgsmlb -> 7


;; This was refactored after part 2
(defn char-of-first-N-uniq
  "Return the 1-based character position where the previous N
   characters were all unique for the first time. If it never
   happens, returns nil."
  [N s]
  (letfn [(dropv [n v]
            "Drops n items from the front of vector v"
            (vec (drop n v)))
          (limit [n vs]
            "Limits the vector vs to at most n entries"
            (if (> (count vs) n)
              (vec (drop (- (count vs) n) vs))
              vs))
          (f [[char-num vals] val]
            "Reducing function that implements the main body.
             Returns the index of the character whose previous 4 are
             first unique.
             Call with [0 []] as the initial accumulator.
             Acc(umulator) = [char-num [a b c d]]
             val(ue) = next character to add
             return: number char-num when b c d val are all different
             or a list, which indicates there is at least one dup in bcdval"
            (let [new-vals (conj (limit (dec N) vals) val)
                  nv-set (set new-vals)]
              #_(print (count nv-set) nv-set "\n") ;; Watch what's going on
              (if (= (count nv-set) N)
                (reduced char-num)
                [(inc char-num) new-vals])))]
    (let [result (reduce f [0 []] s)]
      (if (number? result)
        (inc result)
        nil))))

(defn char-of-first-4-uniq
  "Return the 1-based character position where the previous 4
   characters were all unique for the first time. If it never
   happens, returns nil."
  [s]
  (char-of-first-N-uniq 4 s))

;; Tests
(char-of-first-4-uniq "abcdefghij")
(char-of-first-4-uniq "abcccc")

;; Problem examples
(char-of-first-4-uniq "mjqjpqmgbljsphdztnvjfqwrcgsmlb") ; 7 - first example
(char-of-first-4-uniq "bvwbjplbgvbhsrlpgdmjqwftvncz") ; : first marker after character 5
(char-of-first-4-uniq "nppdvjthqldpwncqszvftbrmjlhg") ; : first marker after character 6
(char-of-first-4-uniq "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ; : first marker after character 10
(char-of-first-4-uniq "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ; : first marker after character 11

;; Part 1 answer:
(char-of-first-4-uniq d6-input-raw)
;; 1100 of 4095

;; Day 6 Part 2 ----------

(char-of-first-N-uniq 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb");: first marker after character 19
(char-of-first-N-uniq 14 "bvwbjplbgvbhsrlpgdmjqwftvncz");: first marker after character 23
(char-of-first-N-uniq 14 "nppdvjthqldpwncqszvftbrmjlhg");: first marker after character 23
(char-of-first-N-uniq 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg");: first marker after character 29
(char-of-first-N-uniq 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw");: first marker after character 26

;; Part 2 answer:
(char-of-first-N-uniq 14 d6-input-raw)
;; 2421


;; Day 7 ---------------------------------------------------------

;; Parse commands to create a tree representation of a directory
;; containing files with sizes.
;; Commands are cd (/,..,dir), ls.
;; ls prints N filename forall the files
;; Commands start with $

(def day7-input
  "Day 7 input file parsed into lines"
  (str/split-lines (slurp "resources/day7-input.txt")))

(def d7p1-test
  "Test input for Day 7 part 1 as a vector of lines"
  (str/split-lines "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"))


(defn get-next-command
  "Returns: [remaining-commands [command args]]
   command is :dir or :ls
   where args is a string for dir and a map of filenames to sizes or :dir"
  [commands]
  (let [cmd-line (first commands)
        commands (vec (rest commands))
        split-cmd (str/split cmd-line, #" ")
        cmd (nth split-cmd 1 nil)]
    ;; Detect errors - we are expecting a command
    (when (not (str/starts-with? cmd-line "$ "))
      (throw (ex-info (str "Invalid command line ($): " cmd-line) {:cmd-line cmd-line})))
    (when (< (count split-cmd) 2)
      (throw (ex-info (str "Invalid command line (length): " cmd-line) {:cmd-line cmd-line})))

    ;; If it's a cd, we're done
    (if (= cmd "cd") ; 0-indexed
      ;; Return the result for CD
      (if (< (count split-cmd) 3)
        (throw (ex-info (str "Invalid command line (cd): " cmd-line) {:cmd-line cmd-line}))
        [commands [:cd (nth split-cmd 2 nil)]])
      ;; Return the result for LS
      (if (not= cmd "ls")
        (throw (ex-info (str "Invalid command line (unknown): " cmd-line) {:cmd-line cmd-line}))
        ;; For LS, we need to slurp commands until the next command
        ;; FIXME: CODE ME
        ))))


;; Massage the input into more useful form one modification at a time
(defn parse-commands
  "Takes a list of lines and turns it into a vector of commands:
   [command args]
   command = cd, args = string dir - one of / .. or <dirname>
   command = ls, args = map of filenames to sizes OR :dir"
  []
  ;; FIXME: CODE ME
  )

(defn add-cwds
  "Takes a list of commands and adds the current directory to each one, as of
   the start of the command and the end of the command.
   [command args] -> [cwd-start cwd-end command args]
   The cwd are in the form [a b c d] containng strings where a is always /.
   Takes as argument the starting directory (presumably [/])."
  []
  ;; FIXME: CODE ME
  )
