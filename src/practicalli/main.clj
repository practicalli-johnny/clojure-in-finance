(ns practicalli.main)

(defn price-generator
  [lower-bound upper-bound]
  (filter (fn [x] (>= x lower-bound))
          (repeatedly (fn [] (rand upper-bound)))))

(comment
 (take 10 (price-generator 12 35))
 )
