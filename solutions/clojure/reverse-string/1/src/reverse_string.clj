(ns reverse-string)

(defn reverse-string [s] ;;
  (apply str (vec (reverse s)))
)
