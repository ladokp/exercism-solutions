(ns bird-watcher)

(def last-week [0 2 5 3 7 8 4])

(defn today [birds] (peek birds))

(defn inc-bird [birds]
  (-> birds
      pop
      (conj (-> birds today inc))))

(defn day-without-birds? [birds] (not (every? pos? birds)))

(defn n-days-count [birds n]
  (->> birds
       (take n)
       (reduce +)))

(defn busy-days [birds]
  (->> birds
       (remove #(< % 5))
       count))

(defn odd-week? [birds]
  (if (every? #{0 1} birds)
    (->> (rest birds)
      (reduce (fn [{:keys [prev]} n]
                {:prev n :res (not= prev n)})
              {:prev (first birds)})
      :res)
    false))
