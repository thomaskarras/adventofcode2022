(ns day5)


(def input (-> 
            (slurp "e:/projects/adventofcode2022/day5/input.txt")
            (clojure.string/split-lines)))


(def initial-input (take 9 input))
(def moves (vec (drop 10 input)))

(def sample (take 5 moves))

(def sample-input "move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def sample-state
  {:1 ["Z" "N"]
   :2 ["M" "C" "D"]
   :3 ["P"]})



(def initialized-cols
  (let [num-cols (-> initial-input
                     vec
                     peek
                     clojure.string/trim
                     (clojure.string/split #"   "))]
    (reduce (fn [all item]
              (assoc all (keyword item) [])) {} num-cols)))

(defn parse-state [state]
  (reduce (fn [parsed line]
            (->> (map-indexed
                  (fn [idx val] {(keyword (str (+ 1 idx))) val}) line)
                 (reduce (fn [tot item]
                           (let [idx (first (keys item))
                                 val (first (vals item))
                                 new-arr (vec (conj (get tot idx) val))]
                             (if (= val "0")
                               tot
                               (assoc tot idx new-arr))))
                         parsed)))
          initialized-cols state))
     

(def cleaned-state 
  (->> initial-input
     (take 8)
     reverse
     (map #(clojure.string/replace-first % #"   " "[0]"))
     (map #(clojure.string/replace % #"\]    " "] [0]"))
     (map #(clojure.string/replace % #"\]    " "] [0]"))
     (map #(clojure.string/replace % #"\]    " "] [0]"))
     (map #(clojure.string/replace % #"[\[\]]" ""))
     (map #(clojure.string/split % #" "))))

(def initial-state 
  (->> cleaned-state
       parse-state))

(defn split-move [move]
  (let [cleaned-move (-> (clojure.string/replace move #"\D" " ")
                         (clojure.string/trim)
                         (clojure.string/split #"\s"))]
    (filter #(not(empty? %)) cleaned-move)))
  

(split-move "move 20 from 7 to 8")

(defn apply-move [state move]
  ;move 3 from 5 to 2
  (let [parsed-str (split-move move)
        numseq (range 0 (Integer/parseInt (first parsed-str)))
        origin (keyword (second parsed-str))
        dest (keyword (last parsed-str))] 
    (println numseq)
    (reduce (fn [curr_state _]
              (let [from (origin curr_state)
                    to (dest curr_state)
                    valToMove (last from)
                    newFrom (if (not-empty from) (pop from) from) 
                    newTo (conj to valToMove)]
                (if (nil? valToMove)
                  curr_state
                  (assoc curr_state origin newFrom dest newTo))))
            state numseq)))
    
(defn apply-move-9001 [state move]
  (let [parsed-str (split-move move)
        numToMove (Integer/parseInt (first parsed-str))
        origin (keyword (second parsed-str))
        dest (keyword (last parsed-str))
        from (origin state)
        to (dest state)
        moving (subvec from (- (count from) numToMove))
        newFrom (vec (take (- (count from) numToMove) from))
        newTo (vec (concat to moving))]
    (assoc state origin newFrom dest newTo)
    ))
            
sample-state
(apply-move-9001 sample-state  "move 2 from 2 to 1")

(defn top-state [state]
  (->> state
       keys
       sort 
       (mapv #(last (% state)))
       (clojure.string/join)))
       
;; part 1
(->> (reduce (fn [curr_state move]
               (apply-move curr_state move)) initial-state moves)
     top-state)
   
;; part 2
(->> (reduce (fn [curr_state move]
               (apply-move-9001 curr_state move)) initial-state moves)
     top-state)


(comment 
  (count [1 2 3])
  (subvec [1 2 3] 2)
  (sort [:3 :2 :1])
  (range 0 1)
  (conj [1 2] [2 3])
  initial-input
  initial-state
  cleaned-state
  moves
  sample 
  (top-state initial-state)
  (pop (get initial-state :1))
  (apply-move initial-state (first sample)))
  
  
  