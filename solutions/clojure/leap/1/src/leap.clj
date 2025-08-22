(ns leap)

(defn leap-year? [year] ;; <- argslist goes here
  (or 
   (and (= (mod year 4) 0) (not= (mod year 100) 0)) 
   (= (mod year 400) 0)
  )
)
