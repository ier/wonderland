(ns alphabet-cipher.coder)

(defn generate-alphabet
  ([]
   (generate-alphabet \a 26))
  ([start len]
   (for [x (range 0 len)]
     (->> x
          (+ (int start))
          char
          str))))

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
      (let [transposed (transpose alphabet)]
        (if (< step (dec length))
          (recur transposed
                 (inc step)
                 (->> transposed
                      (reduce str)
                      (str acc)))
          acc)))))

(defn indexes-of
  [e coll]
  (keep-indexed #(when (= e %2) %1) coll))

(defn index-of
  [e coll]
  (->> coll
       (indexes-of e)
       first))

(defn pick-letter
  [chart alphabet row-code col-code]
  (let [len (count alphabet)
        col (index-of col-code alphabet)
        row (index-of row-code alphabet)
        position (+ (* row len) col)]
    (subs chart position (inc position))))

(defn kwrd
  [keyword times]
  (->> keyword
       cycle
       (take times)))

(defn encode
  [keyword message]
  (let [alphabet (generate-alphabet \a 26)
        chart (generate-chart alphabet)
        keywords (kwrd keyword (count message))]
    (reduce str (map
                 (fn [[row-code col-code]]
                   (pick-letter chart alphabet (str row-code) (str col-code)))
                 (partition 2 (interleave keywords (seq message)))))))

(defn pick-col-name
  [chart alphabet row-code item]
  (let [len (count alphabet)
        position (* (index-of row-code alphabet) len)]
    (->> (+ position len)
         (subs chart position)
         (index-of item)
         (nth alphabet))))

(defn decode
  [keyword message]
  (let [alphabet (generate-alphabet \a 26)
        chart (generate-chart alphabet)
        keywords (kwrd keyword (count message))]
    (reduce str (map
                 (fn [[row-code item]]
                   (pick-col-name chart alphabet (str row-code) item))
                 (partition 2 (interleave keywords (seq message)))))))

(defn decipher
  [cipher message]
  "decypherme")

