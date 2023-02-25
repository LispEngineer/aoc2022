(ns aoc2022.day9
  "Douglas P. Fields, Jr.'s Advent of Code 2022 solutions in Clojure.
   Copyright ⓒ 2023 Douglas P. Fields, Jr. All Rights Reserved.
   symbolics@lisp.engineer"
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; Day 9 Part 1 --------------------------------------------------
;;
;; <DIR> <Steps>
;; R = x + 1
;; L = X - 1
;; U = Y - 1
;; D = Y + 1
;;
;; xNONx
;; N---N
;; O-T-O
;; N---N
;; xNONx
;;
;; Which direction the head moves
;; then the tail follows, if the head is no longer
;; adjacent or on top of the head.
;;
;; Since head cannot move diagonally, it can basically
;; only ever make "knights moves" to get 2 spaces away,
;; or Rook moves, never bishop moves.
;;
;; In the diagram above, the head can never get to the
;; x's while the tail is in T.
;; Note that the diagram is 4-way symmetric.
;;
;; We can just make a simple update matrix for where the
;; tail goes (delta-x-tail, delta-y-tail) given where the head is
;; relative to the tail (delta-x-h2t, delta-y-h2t).

;; xNONx
;; N---N
;; O-T-O
;; N---N
;; xNONx
(def tail-moves
  "See above - where the Tail moves if the Head moves to these places
   relative to the Tail.
   xNONx
   N---N
   O-T-O
   N---N
   xNONx
   --
   Note, the x's are impossible with a single head move.
   However, for a rope (see part 2), they become possible for
   the previous knot to move diagonally twice. So, the knot
   needs to move diagonally too.
   "
  (let [;; Programmatically do our 9 no-tail-moves
        adjacents (for [x [-1 0 1] y [-1 0 1]] [x y])
        adj-map (into {} (map #(vector % [0 0]) adjacents))
        ;; Orthogonal moves of head, orthogonal move of tail
        ortho-map {[-2  0] [-1  0]
                   [ 2  0] [ 1  0]
                   [ 0 -2] [ 0 -1]
                   [ 0  2] [ 0  1]}
        ;; Knights moves of head, diagonal move of tail
        knight-map {[ 2 -1] [ 1 -1] ;; Start at 30° and go counterclockwise
                    [ 1 -2] [ 1 -1]
                    [-1 -2] [-1 -1]
                    [-2 -1] [-1 -1]
                    [-2  1] [-1  1]
                    [-1  2] [-1  1]
                    [ 1  2] [ 1  1]
                    [ 2  1] [ 1  1]}
        ;; Part 2 only - double diagonal moves of previous
        diag-map {[-2 -2] [-1 -1]
                  [-2  2] [-1  1]
                  [ 2 -2] [ 1 -1]
                  [ 2  2] [ 1  1]} ]
    (merge adj-map ortho-map knight-map diag-map)))

(def d9-test-raw
  "Day 9, part 1 test input, raw string."
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def d9-test-lines
  "Day 9, part 1, test input split into lines"
  (str/split-lines d9-test-raw))

(def day9-input
  "Day 9 input file parsed into lines"
  (str/split-lines (slurp "resources/day9-input.txt")))

(defn parse-input-line
  "Turns D # into [:D <num>]"
  [line]
  (let [[sdir snum] (str/split line #" ")
        num (Long/parseLong snum)
        dir (keyword sdir)]
    [dir num]))

(defn expand-direction
  "Expands a [:dir num] to (:dir :dir ... :dir)"
  [[dir num]]
  (repeat num dir))

(def dir-to-delta
  "Maps a :dir to a [delta-x delta-y] value."
  {:U [ 0 -1]
   :D [ 0  1]
   :L [-1  0]
   :R [ 1  0]})

(defn make-full-directions
  "Returns a sequence of single direction moves for the head
   based on the input lines. :R :L :U :D"
  [lines]
  (mapcat expand-direction
    (map parse-input-line lines)))

(defn move-ht
  "Given a [head-x head-y] [tail-x tail-y] current position for the
   head and tail, and a :dir to go, returns the new positions of both."
  [h t dir]
  (let [;; Where the head is moving to
        delta-h   (dir-to-delta dir)
        ;; Where the head now is
        new-h     (mapv + h delta-h)
        ;; Where the new head is relative to current tail
        delta-h2t (mapv - new-h t)
        ;; How the tail should move now
        delta-t   (tail-moves delta-h2t)
        ;; And now the final tail
        new-t     (mapv + t delta-t)]
    [new-h new-t]))

;; Tests
(move-ht [0 0] [0 0] :U)  ;; => [[0 -1] [0 0]]
(move-ht [0 -1] [0 0] :U)  ;; => [[0 -2] [0 -1]]
(move-ht [0 -1] [0 0] :L)  ;; => [[-1 -1] [0 0]]
(move-ht [-1 -1] [0 0] :U)  ;; => [[-1 -2] [-1 -1]]

(defn process-moves
  "Does the actual processing of moves (seq of :dir), returning the set of all
   positions that the tail has been in. Assumes they both start at [0 0],
   but doesn't matter where they start for part 1."
  [moves]
  (letfn [;; Our reducer function takes the old head & tail positions,
          ;; and the set of positions seen by the tail, and returns the
          ;; new version of that given a move.
          (r [[h t t-seen] dir]
              (let [[nh nt] (move-ht h t dir)]
                [nh nt (conj t-seen nt)]))]
    (last (reduce r [[0 0] [0 0] #{[0 0]}] moves))))

;; Test
(count (process-moves (make-full-directions d9-test-lines)))
;; => 13 (correct)

(count (process-moves (make-full-directions day9-input)))
;; => 6337 (correct)

;; -----------------------------------------------------------------
;; Day 9 Part 2

;; THis is like the above, but this time with a 10-knot rope.
;; Let's represent the rope as follows:
;; [[hx hy] [1x 1y] ... [8x 8y] [tx ty]]
;; Where each one is the position of one "knot" of the rope,
;; consisting of 10 knots.
;;
;; Unlike in Part 1, where the head could only make double
;; orthographic or knights moves, a knot could actually make
;; a double diagonal move as well, and the move delta map has
;; to be updated to account for that.

(def d9p2-test-raw
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def d9p2-test-lines (str/split-lines d9p2-test-raw))

(def rope-start
  "Starting position for the rope"
  (vec (repeat 10 [0 0])))

(defn update-rope
  "Updates the new position of the rope assuming
   the head moves :dir.
   This consists of:
   1. Update head directly
   2. Update each pair starting at h+1 [h h+1]
   "
  [rope dir]
  (let [h (first rope)
        t (rest rope)
        ;; Where the head is moving to
        delta-h   (dir-to-delta dir)
        ;; Where the head now is
        new-h     (mapv + h delta-h)]
    ;; Now, we update each entry in t with the previous one
    (reduce (fn [acc cur]
              (let [prev (last acc)
                    newcur
                    ;; See move-ht for this below exploded out
                    (mapv + cur (tail-moves (mapv - prev cur)))]
                (conj acc newcur)))
      [new-h] t)))

;; Test (insufficient test, but whatever.)
(update-rope rope-start :R)
;; => [[1 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]
(update-rope (update-rope rope-start :R) :R)
;; => [[2 0] [1 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]
(update-rope (update-rope (update-rope rope-start :R) :R) :R)
;; => [[3 0] [2 0] [1 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]

(defn process-rope-moves
  "Does the actual processing of moves (seq of :dir), returning the set of all
   positions that the tail has been in. Assumes they both start at [0 0],
   but doesn't matter where they start for part 1."
  [moves]
  (letfn [;; Our reducer function takes the old head & tail positions,
          ;; and the set of positions seen by the tail, and returns the
          ;; new version of that given a move.
          (r [[rope t-seen] dir]
            (let [new-rope (update-rope rope dir)]
              #_(println new-rope)
              [new-rope (conj t-seen (last new-rope))]))]
    (last (reduce r [rope-start #{(last rope-start)}] moves))))

;; Test
(process-rope-moves (make-full-directions d9-test-lines))
;; => #{[0 0]}

;; More detailed test
(def final-set (process-rope-moves (make-full-directions d9p2-test-lines)))
(count final-set) ;; expected 36
;; Print the pic of where the tail has been
(let [[min-x min-y] (reduce #(map min %1 %2) final-set)
      [max-x max-y] (reduce #(map max %1 %2) final-set)]
  (dorun
    (for [y (range min-y (inc max-y))
          x (range min-x (inc max-x))]
      (do
        (if (final-set [x y])
          (print "#")
          (print "."))
        (when (= x max-x) (println))))))
;#.....................
;#.............###.....
;#............#...#....
;.#..........#.....#...
;..#..........#.....#..
;...#........#.......#.
;....#......#.........#
;.....#..............#.
;......#............#..
;.......#..........#...
;........#........#....
;.........########.....

;; Final answer for Day 9 Part 2
(count (process-rope-moves (make-full-directions day9-input)))
;; => 2455 (correct)