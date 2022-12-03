(ns aoc2022.core
  "Douglas P. Fields, Jr.'s Advent of Code 2022 solutions in Clojure.
   Copyright 2022 Douglas P. Fields, Jr. All Rights Reserved.
   symbolics@lisp.engineer")

;; IntelliJ hotkeys on Windows with Cursive:
;; Alt-Shift-P - Send form to REPL
;; Control-\ - Jump to REPL input

;; Day 1 - https://adventofcode.com/2022/day/1

(def day1-input
  "Day 1 input file parsed into lines"
  (clojure.string/split-lines (slurp "resources/day1-input.txt")))

(def d1-elves
  "Turns the day1-input into sums for each elf."
  (letfn [(f [acc val]
            "If val is non-blank, treat it as a string'd long and add it to the last
             item in the acc vector. If it is blank, we're done with the previous value
             so we should start a new one (at 0)."
            (if (clojure.string/blank? val)
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
  (clojure.string/split-lines (slurp "resources/day2-input.txt")))

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
