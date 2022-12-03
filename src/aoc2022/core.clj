(ns aoc2022.core
  "Douglas P. Fields, Jr.'s Advent of Code 2022 solutions in Clojure.
   Copyright 2022 Douglas P. Fields, Jr. All Rights Reserved.
   symbolics@lisp.engineer")

;; IntelliJ hotkeys on Windows with Cursive:
;; Alt-Shift-P - Send form to REPL

;; Day 1 - https://adventofcode.com/2022/day/1

(def day1-input
  "Day 1 input file parsed into lines"
  (clojure.string/split-lines (slurp "resources/day1-input.txt")))

(def d1-elves
  "Turns the day1-input into sums for each elf."
  (letfn [(f [acc val]
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


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
