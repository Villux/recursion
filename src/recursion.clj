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
    (apply max a-seq)))


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
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))



(defn power [n k]
  (cond
   (zero? k) 1
   (= k 1) n
   (= n 0) 0
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (< 0 n 3) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (conj (my-range (dec up-to)) (dec up-to))))


(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (map reverse (tails (reverse a-seq)))))


(defn rotations [a-seq]
  (if (< 0 (count a-seq))
    (map (fn [n _] (concat (drop n a-seq) (take n a-seq))) (iterate inc 0) a-seq)
    (conj (into () a-seq) ())))


(defn my-frequencies-helper [freqs a-seq]
  (conj freqs (frequencies a-seq)))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (mapcat (fn [point] (repeat (get point 1) (get point 0))) (seq a-map)))

(defn my-take [n coll]
  (take n coll))

(defn my-drop [n coll]
  (drop n coll))

(defn halve [a-seq]
  (let [halve (int (/ (count a-seq) 2))]
    (into [] (split-at halve a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [seq-merge-helper (fn [a-seq b-seq sorted-seq]
                           (cond
                            (empty? a-seq) (concat sorted-seq b-seq)
                            (empty? b-seq) (concat sorted-seq a-seq )
                            :else
                              (let [seq1 (first a-seq) seq2 (first b-seq)]
                                (if (< seq1 seq2)
                                  (recur (rest a-seq) b-seq (concat sorted-seq [seq1]))
                                  (recur a-seq (rest b-seq) (concat sorted-seq [seq2]))))))]
    (seq-merge-helper a-seq b-seq '())))



(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [ [first-seq second-seq] (halve a-seq) ]
      (seq-merge (merge-sort first-seq) (merge-sort second-seq)))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

