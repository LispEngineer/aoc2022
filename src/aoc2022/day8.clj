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

;; ------------------------------------------------------
;; Part 2
;; https://adventofcode.com/2022/day/8#part2
;;
;; We need to project from any square in all four directions,
;; that is, given a coordinate [x y] we need to know the numbers
;; from that coordinate out to the edges in each of the four directions.
;;
;; The big question I have is, if you see like this:
;; 5 4 1 5
;; And you're at the leftmost 5, can you really see the 1?
;; What about
;; 5 4 1 4 1 4 1 4 1 5
;; can you really see all those 1s and 4s?
;;
;; The instructions say
;; "stop if you reach an edge
;;  OR at the first tree that is
;;   the same height or taller than the tree under consideration"
;;
;; So it seems that it really allows the 541414141 situation.


(defn projection-value
  "Given a projection - a seq starting with the \"home\" tree,
   how many trees can we see from here? Any tree at the home
   height or higher blocks any further trees."
  [ray]
  (if (< (count ray) 2)
    ;; There are no trees beyond us in this direction, we must
    ;; be on the edge, so we see zero tree.
    0
    ;; ... Count as far as we can see
    (let [home (first ray)]
      (reduce (fn [cnt t]
                (if (>= t home)
                  ;; We can't see beyond this tree; we saw this tree and all
                  ;; the previous trees.
                  (reduced (inc cnt))
                  ;; We saw this tree, and any future trees to come.
                  (inc cnt)))
            0 (rest ray)))))

;; Tests
(projection-value [])
(projection-value [5])
(projection-value [5 4])
(projection-value [5 5])
(projection-value [5 6])
(projection-value [5 5 5])
(projection-value [5 4 5])
(projection-value [5 4 4])
(projection-value [5 4 4 7 3 3 3 3])

(defn senic-score
  "We need to project from any square in all four directions,
   that is, given a coordinate [x y] we need to know the numbers
   from that coordinate out to the edges in each of the four directions.
   --
   m = input matrix - a square nested set of vectors
   [x y] = coordinate; x = column, y = row
     with 0 = leftmost column; 0 = topmost row
   --
   We then map the `projection-value` of each of those four projections,
   and multiply them all together to return the `senic score`.
   --
   If x or y are OOB this will throw an IOOBE.
   "
  [m [x y]]
  (let [row (nth m y) ;; Get the row y
        col (map #(nth % x) m) ;; Get the column x
        ;; _ (println "coord:" [x y] "\nrow:" row "\ncol:" col)
        ;; Now, project forward and backward along the row,
        ;; always keeping the original number in the head position.
        fwd  (drop x row)
        rev  (reverse (take (inc x) row))
        down (drop y col)
        up   (reverse (take (inc y) col))
        ;; _ (println "fwd:" fwd "\nrev:" rev "\ndown:" down "\nup:" up)
        ]
    (apply * (map projection-value [fwd rev down up]))))

;; Tests
(senic-score d8p1-test-parsed [2 1]) ;; => 4
(senic-score d8p1-test-parsed [2 3]) ;; => 8

(defn highest-senic-score
  "Find the senic score for every tree, and return the maximum."
  [m]
  (let [n-rows (count m)
        n-cols (count (first m))
        scores (for [y (range n-rows) ;; This will increment less quickly than
                     x (range n-cols)] ;; this coordinate will
                 (senic-score m [x y]))]
    #_(println (partition n-cols scores))
    ;; These two are semantically equivalent
    #_(reduce max scores)
    (apply max scores)))


;; Test
(highest-senic-score d8p1-test-parsed)
;; => 8 (correct)

;; And get our answer
(highest-senic-score day8-parsed)
;; => 268912 (correct)

