(ns practicalli.main)

(defn price-generator
  [lower-bound upper-bound]
  (filter (fn [price] (>= price lower-bound))
          (repeatedly (fn [] (rand upper-bound)))))

(comment
  (take 10 (price-generator 12 35))
  #_())
   ; nil
 ;;=> (17.384956196113468 33.68757175325423 13.834110458306588 28.527285050749377
 ;;    21.065849733800246 20.5549757596255 20.89116879298458 33.259478300 54419
 ;;    19.787085361486877 12.710363246528475)


(def pricelist (price-generator 12 35))

(comment
  (take 24 (map (fn [price] {:price price}) pricelist))

  (def pricelist (price-generator 12 35))
  ;; => ({:price 13.085529364425375} {:price 17.87854787248638} {:price 32.57151251888454} {:price 21.720996161462907} {:price 34.59582217097654} {:pr
  ;; ice 17.812328994037287} {:price 15.263363233819662} {:price 18.624983417979486} {:price 21.80576125521034} {:price 23.387520869775354} {:price
  ;;  15.704247095413745} {:price 22.89403696276269} {:price 30.381728903307653} {:price 29.45728125568495} {:price 18.384210648686718} {:price 34.
  ;; 389298769828955} {:price 29.62320782024044} {:price 34.109078784142696} {:price 26.091233351092722} {:price 26.935759110018587} {:price 31.756
  ;; 223399146} {:price 23.416599956443356} {:price 14.724727029997673} {:price 27.296160496608515})

  (take 24
        (map
         (fn [time price] [time price])
         (map (fn [time] {:time time})
              (iterate inc 0))
         (map (fn [price] {:price price}) pricelist)))

;;=> ([{:time 0} {:price 13.085529364425375}]
  ;;    [{:time 1} {:price 17.87854787248638}]
  ;;    [{:time 2} {:price 32.57151251888454}]
  ;;    [{:time 3} {:price 21.7 20996161462907}]
  ;;    [{:time 4} {:price 34.59582217097654}]
  ;;    [{:time 5} {:price 17.812328994037287}]
  ;;    [{:time 6} {:price 15.263363233819662}]
  ;;    [{:time 7} {:price 18.624983417979486}]
  ;;    [{:time 8} {:price 21.80576125521034}]
  ;;    [{:time 9} {:price 23.387520869775354}]
  ;;    [{:time 10} {:price 15.704247095413745}]
  ;;    [{:time 11} {:price 22.89403696276269}]
  ;;    [{:time 12} {:price 30.381728903307653}]
  ;;    [{:time 13} {:price 29.45728125568495}]
  ;;    [{:time 14} {:price 18.384210648686718}]
  ;;    [{:time 15} {:price 34.389298769828955}]
  ;;    [{:time 16} {:price 29.62320782024044}]
  ;;    [{:time 17} {:price 34.109078784142696}]
  ;;    [{:time 18} {:price 26.091233351092722}]
  ;;    [{:time 19} {:price 26.935759110018587}]
  ;;    [{:time 20} {:price 31.756223399146}]
  ;;    [{:time 21 } {:price 23.416599956443356}]
  ;;    [{:time 22} {:price 14.724727029997673}]
  ;;    [{:time 23} {:price 27.296160496608515}])

  #_())

;; The results should be a vector of hash-maps,
;; with each hash-map containing :time and :price

(comment
  (take 24
        (->>
         (map
          (fn [time price] [time price])
          (map (fn [time] {:time time})
               (iterate inc 0))
          (map (fn [price] {:price price}) pricelist))
         (map (fn [x] (merge (first x (second x)))))))

  #_())

(defn generate-timeseries
  [pricelist]
  (map (fn [time price]
         {:time time :price price})
       (iterate inc 0)
       pricelist))

(comment

  (take 10 (generate-timeseries (price-generator 12 15)))
; ({:time 0, :price 12.698824778274828}
;  {:time 1, :price 14.726449017908335}
;  {:time 2, :price 13.569970825099581}
;  {:time 3, :price 12.473676606321385}
;  {:time 4, :price 14.120367408951752}
;  {:time 5, :price 12.32087078212636}
;  {:time 6, :price 12.330979977752783}
;  {:time 7, :price 13.092688231924154}
;  {:time 8, :price 12.149629524009198}
;  {:time 9, :price 13.11348079574269})
  
  #_())

(defn random-in-range
  [lower upper]
  (let [value (rand upper)]
    (if (>= value lower)
      value 
     (+ (rand (- upper lower)) lower))))


  (defn stochastic-k
    [last-price low-price hight-price]
    (let [highrange (- hight-price low-price)
          himidpoint (/ highrange 2)
          numerator (if (> last-price himidpoint)
                     (- last-price himidpoint)
                     (- himidpoint low-price))]
      (/ numerator highrange)))


(defn break-local-minima-maxima
  [k]
  (as-> k k 
    (if (<= (int (+ 0.95 k)) 0)
      (+ 0.15 k) 
      k)
    (if (>= k 1)
     (- k 0.15)
     k)))
