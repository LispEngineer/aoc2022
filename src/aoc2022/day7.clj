(ns aoc2022.day7
  "Douglas P. Fields, Jr.'s Advent of Code 2022 solutions in Clojure.
   Copyright 2023 Douglas P. Fields, Jr. All Rights Reserved.
   symbolics@lisp.engineer"
  (:require [clojure.set :as set]
            [clojure.string :as str]))

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

(def ^:const cmd-prompt
  "The command prompt"
  "$ ")

(defn parse-dir-entry
  "Parses a directory entry. It consists of two tokens separated by a space.
   The first is either `dir` (meaning it's a directory) or a number which is
   a file size. The second is the name of the dir/file.
   --
   Returns a map of this template:
   {:name ___ :type {:dir,:file} :size ___}
   Size will be -1 for directories.
   Directories also have another key :entries {} which will be
   indexed by the file/dir name.
   "
  [entry]
  (let [split (str/split entry #" " 2)
        [size name] split]
    #_(println "Split:" split)
    #_(println "Size:" size)
    #_(println "Name:" name)
    (when (not= 2 (count split))
      (throw (ex-info (str "Invalid dir entry: " entry) {:dir-entry entry})))

    (if (= size "dir")
      {:type :dir :name name :size -1 :entries {}}
      ;; FIXME: Parse the size safely
      {:type :file :name name :size (Long/parseLong size)})))

(def root-dir
  "The empty top level of our nested tree structure we're building."
  {:type :dir :name "/" :size -1 :entries {}})

(defn get-next-command
  "Returns: [remaining-commands [command args]]
   command is :dir or :ls
   where args is a string for dir and a map of filenames to sizes or :dir.
   remaining-commands may be a seq not a vec."
  [commands]
  (let [cmd-line (first commands)
        commands (vec (rest commands))
        split-cmd (str/split cmd-line, #" ")
        cmd (nth split-cmd 1 nil)]
    ;; Detect errors - we are expecting a command
    (when (not (str/starts-with? cmd-line cmd-prompt))
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
        ;; For LS, we need to slurp commands until the next command, which starts with a $
        (let [dir-entries (take-while #(not (str/starts-with? % cmd-prompt)) commands)
              commands (drop (count dir-entries) commands)
              ;; Parse the entries into maps
              parsed-entries (map parse-dir-entry dir-entries)]
          [commands [:ls parsed-entries]])))))

;; Massage the input into more useful form one modification at a time
(defn parse-commands
  "Takes a list of lines and turns it into a vector of commands:
   [command args]
   command = cd, args = string dir - one of / .. or <dirname>
   command = ls, args = map of filenames to sizes OR :dir
   --
   Only call this with the single arg, a seq of terminal output commands.
   The 2-arg version is for internal use only."
  ([commands]
   ;; TODO: Use a volatile vector to the internal and finalize it here instead,
   ;; for performance reasons (if desired)
   (parse-commands commands []))
  ([commands acc]
   ;; Get the next command, and add it to the end of the accumulator,
   ;; then do it again until we are out of commands, and return the
   ;; accumulator.
   (if (empty? commands)
     acc
     (let [[commands command] (get-next-command commands)]
       (recur commands (conj acc command))))))

(def d7p1-test-desired-output
  "The raw tree structure we should build from the day 7 part 1
   test input... So I know what I'm building."
  {:type :dir :name "/" :size -1
         :entries {"a"     {:type :dir, :name "a", :size -1
                            :entries {"e" {:type :dir, :name "e", :size -1
                                           :entries {"i" {:type :file, :name "i", :size 584}}}
                                      "f" {:type :file, :name "f", :size 29116}
                                      "g" {:type :file, :name "g", :size 2557}
                                      "h.lst" {:type :file, :name "h.lst", :size 62596}
                                      }}
                   "b.txt" {:type :file, :name "b.txt", :size 14848514}
                   "c.dat" {:type :file, :name "c.dat", :size 8504156}
                   "d"     {:type :dir, :name "d", :size -1
                            :entries {"j" {:type :file, :name "j", :size 4060174}
                                      "d.log" {:type :file, :name "d.log", :size 8033020}
                                      "d.ext" {:type :file, :name "d.ext", :size 5626152}
                                      "k" {:type :file, :name "k", :size 7214296}

                                      }}
                   }})

(defn handle-cd
  "Changes to the specified directory given the current path.
   The path of the root directory is [].
   Path is a vector containing strings in order from the outermost
   to the innermost."
  [cwd cd-to]
  (cond
    (= cd-to "/")  [] ;; The root directory has no name
    (= cd-to "..") (vec (drop-last cwd)) ;; We could check if we're in root already
    true           (conj cwd cd-to)))

;; Tests
(handle-cd [] "a")
(handle-cd ["a" "b" "c"] "a")
(handle-cd ["a" "b" "c"] "..")
(handle-cd ["a" "b" "c"] "/")

(defn make-dir-path
  "Makes a path for update-in or assoc-in that
   reflects the way our nested map structure looks,
   namely that it has `:entries \"dir\"` to get to the
   specified directory.
   --
   Returns a seq."
  [cwd]
  (mapcat #(list :entries %) cwd))

;; Tests
(make-dir-path [])
(make-dir-path ["a"])
(make-dir-path ["a" "b" "c"])
(get-in desired-output (make-dir-path ["a"]))
(get-in desired-output (make-dir-path ["a" "e"]))
(get-in desired-output (make-dir-path ["a" "e" "i"]))
(get-in desired-output (make-dir-path ["a" "e" "i" "o"])) ;; is nil, no such entry

(defn make-entries
  "Makes an entries map of the list of files provided."
  [fl] ;; File list
  (reduce #(assoc %1 (:name %2) %2) {} fl))

(defn build-dir
  "Builds the directory listing to be like d7p1-test-desired-output format,
   one command at a time.
  "
  [commands]
  (let [parsed-cmds (parse-commands commands)
        cwd (atom [])
        dir (atom root-dir)]
    ;; This is very imperative and not idiomatic clojure, but
    ;; we can do it a different way in the future if desired.
    ;; (Use a reducer that uses the two atoms as the accumulator,
    ;; and return the dir.)
    ;; For each command, update the CWD or dir and then do the
    ;; next command.
    (doseq [cmd parsed-cmds]
      #_(println "Cmd:" cmd)
      (let [[cmd arg] cmd]
        (if (= cmd :cd)
          ;; Update our path
          (swap! cwd handle-cd ,,, arg)
          ;; Add files to our directory.
          ;; We need to update the :entries entry of the appropriate
          ;; place in the directory structure.
          ;; FIXME: Not arg, but the parsed version of the arg
          (swap! dir assoc-in ,,, (concat (make-dir-path @cwd) '(:entries)) (make-entries arg))))
      #_(println "NewCWD:" @cwd)
      #_(println "NewDIR:" @dir)
      )
    ;; Return the built directory
    @dir))

;; Test
(= (build-dir d7p1-test) desired-output)

;; -----------------------------------------------------------------------
;; At this point we can build the tree structure of the directories per the
;; information provided, subject to these conditions:
;; 1. It never CDs up past root
;; 2. It never CDs into a previously unseen directory
;; 3. It always lists the contents of a directory before going into
;;    a deeper directory in there.
;; FIXME: I should really build checks in for those things into the code above.

;; Now we have to walk the tree and build the total size of each directory.
;; Directories have their sizes initialized to -1, which is reasonable.
;; What we will do is walk the tree with one copy of the data structure,
;; and update the sizes (in a second copy) since the tree structure will
;; stay the same while we update the size keys.
;; We will need to do a depth-first traversal because we need to calculate
;; the deeper parts before we can add them into the higher parts.
;; This is not a very idiomatic Clojure way of doing it either, sorry.

(defn calc-sizes
  "TODO"
  ([dir]
   ;; Start off the recursive version and return the fully updated tree
   (let [atom-dir (atom dir)]
     (calc-sizes dir [] atom-dir)
     @atom-dir))
  ;; -----
  ([subdir path whole-dir]
   ;; We have three things:
   ;; subdir - the segment of the dir tree we're currently looking at
   ;; path - the path from whole-dir to here (via :entries keys)
   ;; whole-dir - an atom containing the whole tree that we can modify
   #_(println "Path:" path)
   (when (not= :dir (:type subdir))
     (throw (ex-info (str "Internal error, cd into file: " subdir) {:subdir subdir :path path})))
   (doseq [file (vals (:entries subdir))]
     (when (= :dir (:type file))
       ;; Calculate the size of this subdirectory
       #_(println "Calculating size of subdir:" (:name file))
       ;; We cannot `recur`, that is only for tail recursion.
       (calc-sizes file (conj path (:name file)) whole-dir)))
   ;; Now that all the subdirectories have been added,
   ;; add the sizes of all the files/dirs within
   ;; and set that to the size of us.
   #_(println "Now calculating for path:" path)
   (let [children (get-in @whole-dir (concat (make-dir-path path) '(:entries)))
         child-sizes (map (comp :size second) children)
         total-size (reduce + child-sizes)]
     (swap! whole-dir assoc-in ,,, (concat (make-dir-path path) '(:size)) total-size)
     #_(println "Size of" path "is" total-size))
   ;; ...and we're done
   @whole-dir ;; Unnecessary return value
   ))

;; mapping on a map returns pairs [key val]
(map (comp :size second) (:entries desired-output)) ;; (-1 14... 85... -1)

;; Test
(calc-sizes desired-output)
;; Size of [a e] is 584
;; Size of [a] is 94853
;; Size of [d] is 24933642
;; Size of [] is 48381165