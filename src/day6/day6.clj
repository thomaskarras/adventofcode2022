(ns day6
  (:require [clojure.string :as cstr]))


(def input (vec (filter seq (-> 
                             (slurp "src/day6/input.txt")
                             (cstr/split #"")
                             ))))

(def sample ["bvwbjplbgvbhsrlpgdmjqwftvncz"  ; 5
             "nppdvjthqldpwncqszvftbrmjlhg"  ; 6
             "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" ; 10
             "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" ; 11
             ]) 

(defn find-signal [line distinctChars]
  (loop [letters line
         track []
         position 0]
    ;; (println letters)
    ;; (println track)
    ;; (println position)
    (if (empty? letters)
      0 
      (let [newTrack (conj track (first letters))
            newPosition (inc position)]
        (if (= (count newTrack) distinctChars)
          (if (= (count (set newTrack)) distinctChars)
            newPosition
            (recur (subvec letters 1 )
                   (subvec newTrack 1 )
                   newPosition))
          (recur (subvec letters 1)
                 newTrack
                 newPosition))))))

(find-signal (vec (filter seq (cstr/split (second sample) #""))) 4)

(->> 
 (map #(cstr/split % #"") sample)
 (map (fn [line] 
        (vec (filter seq line)))) 
 (map #(find-signal % 4)))


;; part 1
(find-signal input 4)

;; part 2
(find-signal input 14)

(comment 
  sample
  (subvec ["a" "b"] 1 )
  (drop 1 ["a" "b"])
  (conj [] (first ["a"]))
  (drop 1 "abc")
  (conj (cstr/split "abc" #"") "d")
  sample 
  )