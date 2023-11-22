(ns alphabet-cipher.coder)

(defn- generate-alphabet
  [start len]
  (for [x (range 0 len)]
    (->> (+ x (int start))
         char
         str)))

(defn- transpose
  [xs position]
  (->> (split-at position xs)
       reverse
       flatten))

(defn- generate-chart
  [alphabet]
  (let [length (count alphabet)]
    (loop [alphabet alphabet
           step 0
           acc (reduce str (transpose alphabet 0))]
      (let [transposed (transpose alphabet 1)]
        (if (< step (dec length))
          (recur transposed
                 (inc step)
                 (->> (reduce str transposed)
                      (str acc)))
          acc)))))

(defn- index-of
  [e coll]
  (->> coll (keep-indexed #(when (= e %2) %1)) first))

(defn- pick-letter
  [chart row-code col-code]
  (let [len (:alphabet-length chart)
        alphabet (subs (:content chart) 0 len)
        position (->> (count alphabet)
                      (* (index-of row-code alphabet))
                      (+ (index-of col-code alphabet)))]
    (subs (:content chart) position (inc position))))

(defn- pick-col-name
  [chart row-code item]
  (let [len (:alphabet-length chart)
        alphabet (subs (:content chart) 0 len)
        position (* (index-of row-code alphabet) len)]
    (->> (+ position len)
         (subs (:content chart) position)
         (index-of item)
         (nth alphabet))))

(defn- kwrd
  [keyword times]
  (->> keyword
       cycle
       (take times)))

(def aplhabet-length 26)
(def chart {:alphabet-length aplhabet-length
            :content (->> aplhabet-length
                          (generate-alphabet \a)
                          generate-chart)})

(defn encode
  [keyword message]
  (let [keywords (kwrd keyword (count message))
        pairs (partition 2 (interleave keywords message))]
    (reduce str (map
                 (fn [[row-code col-code]]
                   (pick-letter chart row-code col-code))
                 pairs))))

(defn decode
  [keyword message]
  (let [keywords (kwrd keyword (count message))
        pairs (partition 2 (interleave keywords message))]
    (reduce str (map
                 (fn [[row-code item]]
                   (pick-col-name chart row-code item))
                 pairs))))

(defn decipher
  [cipher message]
  (let [pairs (partition 2 (interleave cipher message))
        keywords (reduce str (map
                              (fn [[row-code item]]
                                (pick-col-name chart item row-code))
                              pairs))]
    (loop [cntr 1]
      (let [strings (->> (partition cntr keywords)
                         (map #(reduce str %))
                         distinct)]
        (if (= 1 (count strings))
          (first strings)
          (recur (inc cntr)))))))
