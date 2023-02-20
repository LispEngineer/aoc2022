(ns aoc2022.day8
  "Douglas P. Fields, Jr.'s Advent of Code 2022 solutions in Clojure.
   Copyright 2023 Douglas P. Fields, Jr. All Rights Reserved.
   symbolics@lisp.engineer"
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; Day 8: https://adventofcode.com/2022/day/8


;; In essence, we need to turn a list of numbers
;; into a list of booleans, subject to:
;; In order, remember the highest number seen.
;; (Start from minimum, or -1.)
;; If the current number is higher, then it's true, and update the highest number.
;; IF the current number is less or equal, then it's false, do no update.

(defn visibility
  "In order, remember the highest number seen.
   (Start from minimum, or -1.)
   If the current number is higher, then it's true, and update the highest number.
   IF the current number is less or equal, then it's false, do no update.
   --
   Return the vector of booleans.
   "
  [nums] ;; Vector of numbers
  (first
    ;; Take the "out" of our reducer only
    (reduce (fn [[out highest] val]
              (if (> val highest)
                ;; This one is visible
                [(conj out true) val]
                ;; This one is not visible
                [(conj out false) highest]))
            [[] -1]
            nums)))

;; Now, we'll create a 2-dimensional matrix of these numbers.
;; We will then run the visibility four times:
;;   On each row
;;   On each row backwards
;;   On each column
;;   On each column backwards
;; We can do this by transforming the matrix appropriately,
;; then doing the visibility on each row, then transforming
;; the result back out.
;;
;; Finally, we will take all four of these matrixes and "or"
;; them together, such that any tree visible from any of the
;; four sides is visible, period.

(def d8p1-test-raw
  "Day 8, part 1 test input, raw string."
  "30373
25512
65332
33549
35390")

(def d8p1-test-lines
  "Day 8, part 1, test input split into lines"
  (str/split-lines d8p1-test-raw))

(def day8-input
  "Day 8 input file parsed into lines"
  (str/split-lines (slurp "resources/day8-input.txt")))

(defn parse-line
  "Converts a line into a vector of single-digit integers"
  ;; This is terribly inefficient but easy to understand
  ;; FIXME: Error handling
  [line]
  (mapv #(Long/parseLong (str %)) line))

(def day8-parsed
  "Official input parsed into a 2D vector"
  (mapv parse-line day8-input))
(def d8p1-test-parsed
  "Test input parsed into a 2D vector"
  (mapv parse-line d8p1-test-lines))

(defn transpose
  "Transposes a 2D fully filled matrix (nested vectors)"
  [inm]
  ;; Transpose: For each column, turn it into a row by taking the
  ;; nth item of each row.
  ;; Nested maps are f'ing hard to understand when reading.
  (mapv #(mapv (fn [x] (nth x %)) inm)
        (range (count (first inm)))))

(defn reverse-m
  "Reverses each row of a 2D fully filled matrix (nested vectors)"
  [inm]
  (mapv (comp vec reverse) inm))

(defn make-four-versions
  "Makes 4 versions of the input matrix,
   transposed and reversed."
  [inm]
  (let [original  inm
        ;; Transpose: For each column, turn it into a row by taking the
        ;; nth item of each row.
        ;; Nested maps are f'ing hard to understand when reading.
        transpose (transpose inm)
        rev (reverse-m inm)
        rev-trans (reverse-m transpose)]
    [original transpose rev rev-trans]))

(defn invertability-test
  "Makes sure that the modifications with transpose and reverse can be
   inverted (i.e., we didn't write any bugs)."
  [inm]
  (let [original  inm
        trans     (transpose inm)
        rev       (reverse-m inm)
        rev-trans (reverse-m trans)
        _         (println "o:" original "\nt:" trans "\nr:" rev "\nrt:" rev-trans "\n")
        original  original
        trans     (transpose trans)
        rev       (reverse-m rev)
        rev-trans (transpose (reverse-m rev-trans))
        _         (println "o:" original "\nt:" trans "\nr:" rev "\nrt:" rev-trans "\n")]
    (= inm original trans rev rev-trans)))

(defn full-visibility
  "Calculates the number of trees fully visible.
   Modifies the matrix in all possible ways of transpose/reverse,
   calculates visiblity for each, modifies it back, then sees what
   are visible anywhere."
  [inm]
  (let [original  inm
        trans     (transpose inm)
        rev       (reverse-m inm)
        rev-trans (reverse-m trans)
        ;; _         (println "o:" original "\nt:" trans "\nr:" rev "\nrt:" rev-trans "\n")
        ;; Raw visibilities
        v-o  (mapv visibility original)
        v-t  (mapv visibility trans)
        v-r  (mapv visibility rev)
        v-rt (mapv visibility rev-trans)
        ;; _    (println "vo:" v-o "\nvt:" v-t "\nvr:" v-r "\nvrt:" v-rt "\n")
        ;; Transform those back to original space
        ;; left, right, top, bottom
        l-r  v-o
        t-b  (transpose v-t)
        r-l  (reverse-m v-r)
        b-t  (transpose (reverse-m v-rt))
        ;; _    (println "l-r:" l-r "\nt-b:" t-b "\nr-l:" r-l "\nb-t:" b-t "\n")
        ;; Now, since all we care about is the total number of visible
        ;; trees, we can get rid of the 2D structure, see which are visible,
        ;; and count those.
        ;; or is a macro, so can't use it like a function, sigh
        visibles (map #(or %1 %2 %3 %4) (flatten l-r) (flatten t-b) (flatten r-l) (flatten b-t))
        ;; If we wanted to get it back to a matrix form, we could use the
        ;; `partition` function with the row length.
        ]
    ;; (println "Visibles:" (partition 5 visibles))
    (count (filter identity visibles))))

;; Test
(full-visibility d8p1-test-parsed)
;; => 21 (as expected)

;; Day 8 part 1 answer
(full-visibility day8-parsed)
;; => 1803 (correct)

