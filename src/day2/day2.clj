(ns day2)

(defn get-points1 [val]
  (case val
    ["A" "X"] 4
    ["B" "Y"] 5
    ["C" "Z"] 6
    ["A" "Y"] 8
    ["B" "Z"] 9
    ["C" "X"] 7
    ["A" "Z"] 3
    ["B" "X"] 1
    ["C" "Y"] 2))

; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
; 6 3 0 
(defn get-points2 [val]
  (case val
    ["A" "X"] 3  ;lose with scissors
    ["B" "Y"] 5  ;draw with paper
    ["C" "Z"] 7  ;win with rock 
    ["A" "Y"] 4  ;draw with rock 
    ["B" "Z"] 9  ;win with scissors
    ["C" "X"] 2  ;lose with paper
    ["A" "Z"] 8  ;win with paper
    ["B" "X"] 1  ;lose with rock
    ["C" "Y"] 6)) ;draw with scissors

(let [input (->> (slurp "src/day2/input.txt")
                (clojure.string/split-lines)
                (map #(clojure.string/split % #" ")))
      ]
  
  (comment 
    """
       A & X = Rock 1
       B & Y = Paper 2 
       C & Z = Scissors 3
       6, 3, 0
       """)
  
  (->> input
       (map get-points2)
       (reduce (fn[x y] (+ x y))))
)
  

(comment
  (-> (java.io.File. ".") .getAbsolutePath)
  (clojure.string/split "C Z" #" ")
  (get-points2 ["C" "Y"]))