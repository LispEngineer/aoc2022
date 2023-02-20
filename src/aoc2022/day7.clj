(ns aoc2022.day7
  "Douglas P. Fields, Jr.'s Advent of Code 2022 solutions in Clojure.
   Copyright 2022 Douglas P. Fields, Jr. All Rights Reserved.
   symbolics@lisp.engineer"
  (:require [clojure.set :as set]
            [clojure.string :as str]))


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


(defn parse-commands'
  "TODO"
  [commands so-far]
  (println "Commands:" commands)
  (println "So far:" so-far)
  (if (empty? commands)
    ;; Final case
    so-far
    ;; Recurring case
    (let [[remaining next-command] (get-next-command commands)]
      (println "Remaining: " remaining)
      (println "Next cmd: " next-command)
      (recur remaining (conj so-far next-command)))))

;; Massage the input into more useful form one modification at a time
(defn parse-commands
  "Takes a list of lines and turns it into a vector of commands:
   [command args]
   command = cd, args = string dir - one of / .. or <dirname>
   command = ls, args = map of filenames to sizes OR :dir"
  ;; Unary: Starts the recursive function
  ([commands]
   (parse-commands [commands []]))
  ([[commands so-far]]

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
