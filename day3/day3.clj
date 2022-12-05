(ns day3)


(def point-mapping 
  (let [lowercase (map char (range 97 123))
        uppercase (map char (range 65 91))]
    (-> (concat lowercase uppercase)
        (zipmap (range 1 53)))))

(def input (-> 
            (slurp "e:/projects/adventofcode2022/day3/input.txt")
            (clojure.string/split-lines)))

(let [row (split-at (/ (alength (to-array (first input))) 2) (first input))]
  (clojure.set/intersection (set (first row)) (set (second row))))

; part 1
(->> input
     (map #(split-at (/ (alength (to-array %)) 2) %))
     (map #(clojure.set/intersection (set (first %)) (set (second %))))
     (map first)
     (map #(get point-mapping %))
     (reduce (fn [total x] (+ total x)))
     )

; part 2
(->> input
     (map to-array)
     (map set)
     (partition 3)
     (map #(apply clojure.set/intersection %))
     (map first)
     (map #(get point-mapping %))
     (reduce (fn [total x] (+ total x))))

(comment 
  (concat ["a"] ["b"])
  (apply clojure.set/intersection [#{"a"} #{"b", "a"}] )
  (get point-mapping \a)
  point-mapping)