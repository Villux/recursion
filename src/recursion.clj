(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (reduce * coll)))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))


(defn my-last [coll]
  (last coll))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max a-seq)))

(count [1 2])
(sort '([1 10] [3 4] [1 1]))

(defn seq-max [seq-1 seq-2]
  (second (sort (cons seq-1 (cons seq-2 [])))))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (filter pred? a-seq))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (take-while pred? a-seq))


(defn my-drop-while [pred? a-seq]
  (drop-while pred? a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(my-map + [1 2 3] [4 4 4]) ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] []) ;=> ()


(defn power [n k]
  :-)

(defn fib [n]
  :-)

(defn my-repeat [how-many-times what-to-repeat]
  [:-])

(defn my-range [up-to]
  [:-])

(defn tails [a-seq]
  [:-])

(defn inits [a-seq]
  [:-])

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

