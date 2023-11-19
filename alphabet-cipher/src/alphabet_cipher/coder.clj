(ns alphabet-cipher.coder)

(defn generate-alphabet
  ([]
   (generate-alphabet \a 26))
  ([start len]
   (for [x (range 0 len)]
     (->> start
          int
          (+ x)
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

(defn decode
  [keyword message]
  (let [len 26
        alphabet (generate-alphabet \a len)
        chart (generate-chart alphabet)
        keywords (kwrd keyword (count message))]
    (reduce str (map
                 (fn [[row-code item]]
                   (let [idx (index-of (str row-code) alphabet)
                         position (* idx len)
                         row (subs chart position (+ position len))
                         i (index-of item row)]
                     (nth alphabet i)))
                 (partition 2 (interleave keywords (seq message)))))))

(defn decipher
  [cipher message]
  "decypherme")

