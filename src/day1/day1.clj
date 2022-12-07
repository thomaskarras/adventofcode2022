(ns day1)


(let [input (-> (slurp "src/day1/input.txt")
                (clojure.string/split-lines))
      totals 
      (->> 
       (partition-by empty? input)
       (filter #(when (seq (first %)) %))
       (map (fn [x] (map #(Integer/parseInt %) x) ))
       (map (fn [x] (reduce (fn [all, val] (+ all val)) x)))
       )]
  (def all-totals totals)
  (def top (apply max totals))
  (def top3 (->> totals
                 (sort >)
                 (take 3))) 
  (def top3Totals (reduce (fn [total x] (+ total x)) top3)))


(comment
  (-> (java.io.File. ".") .getAbsolutePath)
  all-totals
  top
  top3
  top3Totals)