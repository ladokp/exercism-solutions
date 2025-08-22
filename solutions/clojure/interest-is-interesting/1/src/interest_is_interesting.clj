(ns interest-is-interesting)

(defn interest-rate
  "Calculate the interest rate"
  [balance]
    (cond (< balance 0) -3.213
          (< balance 1000) 0.5
          (< balance 5000) 1.621
          :else 2.475
    )
  )

(defn annual-balance-update
  "Calculate the annual balance update"
  [balance]
     (let [diff (* (.abs balance) (bigdec (/ (interest-rate balance) 100.0)))]
     (+ balance diff))
  )

(defn amount-to-donate
  "Calculate how much money to donate"
  [balance tax-free-percentage]
    (if (neg? balance)
      0
      (int (* 2 balance (/ tax-free-percentage 100))))
  )