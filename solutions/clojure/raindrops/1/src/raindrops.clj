(ns raindrops)

(defn convert [num]
  (cond
    (= 0  (mod num 105)) "PlingPlangPlong"
    (= 0  (mod num  35)) "PlangPlong"
    (= 0  (mod num  21)) "PlingPlong"
    (= 0  (mod num  15)) "PlingPlang"
    (= 0  (mod num   7)) "Plong"
    (= 0  (mod num   5)) "Plang"
    (= 0  (mod num   3)) "Pling"
    :else (str num)))