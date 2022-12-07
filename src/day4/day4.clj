(ns day4)


(def input (-> 
            (slurp "src/day4/input.txt")
            (clojure.string/split-lines)))


(def sample (take 5 input))

; "28-47" -> [28 47]
(defn parse-single [x]
  (->> (clojure.string/split x #"-")
       (map #(Integer/parseInt %)))) 

; "28-47,45-47" -> [[28 47] [45 47]] 
(defn parse [x]
  (->> (clojure.string/split x #",")
       (map parse-single)))

(defn compare-pair [pair]
  (let [x (first pair)
        x1 (first x)
        x2 (second x)
        y (second pair)
        y1 (first y)
        y2 (second y)]
    (cond 
      (and (>= x1 y1) (<= x2 y2)) 1
      (and (>= y1 x1) (<= y2 x2)) 1
      :else 0
      )
    )
  )

(defn compare-overlap-pair [pair]
  (let [x (first pair)
        x1 (first x)
        x2 (second x)
        y (second pair)
        y1 (first y)
        y2 (second y)]
    (cond
      (or (and (>= x1 y1) (<= x1 y2)) (and (>= x2 y1) (<= x2 y2))) 1
      (or (and (>= y1 x1) (<= y1 x2)) (and (>= y2 x1) (<= y2 x2))) 1
      :else 0)))

; part 1
(->> (map parse input)
     (map compare-pair)
     (reduce (fn[total x] (+ total x))))

; part 2
(->> (map parse input)
     (map compare-overlap-pair)
     (reduce (fn [total x] (+ total x))))



(comment 
  sample 
  )