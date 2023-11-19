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

(defn generate-chart []
  (let [start \a
        length 26
        alphabet (generate-alphabet start length)]
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

(defn pick-letter
  [chart row col]
  )

(pick-letter (generate-chart) "s" "m")


(defn encode
  [keyword message]
  "encodeme")

(defn decode
  [keyword message]
  "decodeme")

(defn decipher
  [cipher message]
  "decypherme")

