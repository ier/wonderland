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

(defn encode
  [keyword message]
  (let [alphabet (generate-alphabet \a 26)
        chart (generate-chart alphabet)
        keywords (take (count message) (cycle keyword))]
    (reduce str (map
                 (fn [[row col]] (pick-letter chart alphabet (str row) (str col)))
                 (partition 2 (interleave keywords (seq message)))))))

(defn decode
  [keyword message]
  "decodeme")

(defn decipher
  [cipher message]
  "decypherme")

