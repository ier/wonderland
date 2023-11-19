(ns alphabet-cipher.coder)

(defn generate-alphabet
  ([]
   (generate-alphabet \a 26))
  ([start len]
   (for [x (range 0 len)]
     (str (char (+ (int start) x))))))

(defn transpose
  ([xs]
   (transpose xs 1))
  ([xs position]
   (->> xs
        (split-at position)
        reverse
        flatten)))

(defn generate-chart
  [alphabet]
  (let [length (count alphabet)]
    (loop [alphabet alphabet
           step 0
           acc (reduce str (transpose alphabet 0))]
      (let [transposed (transpose alphabet)
            row (reduce str transposed)]
        (if (< step (dec length))
          (recur transposed
                 (inc step)
                 (str acc row))
          acc)))))

(defn indexes-of
  [e coll]
  (keep-indexed #(when (= e %2) %1) coll))

(defn index-of
  [e coll]
  (first (indexes-of e coll)))

(defn pick-letter
  [chart alphabet row-code col-code]
  (let [len (count alphabet)
        col (index-of col-code alphabet)
        row (index-of row-code alphabet)
        position (+ (* row len) col)]
    (subs chart position (inc position))))

(comment
 (let [start \a
       length 26
       alphabet (generate-alphabet start length)
       chart (generate-chart alphabet)]
   (pick-letter chart alphabet "s" "m")
   (pick-letter chart alphabet "z" "b"))
 )


(defn encode
  [keyword message]
  "encodeme")

(defn decode
  [keyword message]
  "decodeme")

(defn decipher
  [cipher message]
  "decypherme")

