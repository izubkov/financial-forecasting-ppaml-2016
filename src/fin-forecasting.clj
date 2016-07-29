;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns financial-forecasting
  (:require [gorilla-plot.core :as plot]
            [anglican lmh]
            [anglican.stat :as stat])
  (:use [anglican core emit runtime
         [inference :refer [infer equalize]]]))

(load "data")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defdist log-normal 
   "a lognormal distribution"
  [mu sigma] [n (normal mu sigma)]
  (sample* [this]
    (exp (sample* n)))
  (observe* [this v]
            (observe* n (log v) )))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@b37528&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@b37528>"}
;; <=

;; @@
 (def samples
  (repeatedly 10000 #(sample* (normal 1 0.5))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/samples</span>","value":"#'financial-forecasting/samples"}
;; <=

;; @@
(def log-samples
  (repeatedly 10000 #(sample* (log-normal 1 0.5))))

(plot/compose
  (plot/histogram samples :normalise
                              :probability-density
                              :bins 80
                              :plot-range [[-5 5] [0 0.8]])
  (plot/histogram log-samples :normalise
                              :probability-density
                              :bins 40
                              :plot-range [[-5 5] [0 0.8]]))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-5,5]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"ace83c27-5118-4297-a23f-db64e1b92902","values":[{"x":-5.0,"y":0},{"x":-4.875,"y":0.0},{"x":-4.75,"y":0.0},{"x":-4.625,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.375,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.125,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.875,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.625,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.375,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.125,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.875,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.625,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.375,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.125,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.875,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.625,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.375,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.125,"y":0.0},{"x":-1.0,"y":8.0E-4},{"x":-0.875,"y":0.0016},{"x":-0.75,"y":0.0016},{"x":-0.625,"y":0.0024},{"x":-0.5,"y":0.008},{"x":-0.375,"y":0.0096},{"x":-0.25,"y":0.0264},{"x":-0.125,"y":0.048},{"x":0.0,"y":0.084},{"x":0.125,"y":0.1424},{"x":0.25,"y":0.216},{"x":0.375,"y":0.3152},{"x":0.5,"y":0.4088},{"x":0.625,"y":0.564},{"x":0.75,"y":0.6376},{"x":0.875,"y":0.7832},{"x":1.0,"y":0.82},{"x":1.125,"y":0.78},{"x":1.25,"y":0.6928},{"x":1.375,"y":0.6344},{"x":1.5,"y":0.5528},{"x":1.625,"y":0.4048},{"x":1.75,"y":0.3024},{"x":1.875,"y":0.2336},{"x":2.0,"y":0.1504},{"x":2.125,"y":0.0936},{"x":2.25,"y":0.0424},{"x":2.375,"y":0.0208},{"x":2.5,"y":0.0088},{"x":2.625,"y":0.0056},{"x":2.75,"y":0.0072},{"x":2.875,"y":0.0},{"x":3.0,"y":8.0E-4},{"x":3.125,"y":0.0},{"x":3.25,"y":0.0},{"x":3.375,"y":0.0},{"x":3.5,"y":0.0},{"x":3.625,"y":0.0},{"x":3.75,"y":0.0},{"x":3.875,"y":0.0},{"x":4.0,"y":0.0},{"x":4.125,"y":0.0},{"x":4.25,"y":0.0},{"x":4.375,"y":0.0},{"x":4.5,"y":0.0},{"x":4.625,"y":0.0},{"x":4.75,"y":0.0},{"x":4.875,"y":0.0},{"x":5.0,"y":0.0},{"x":5.125,"y":0.0},{"x":5.25,"y":0}]},{"name":"8f414a15-34b9-4928-882d-a347fef58e78","values":[{"x":-5.0,"y":0},{"x":-4.75,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.75,"y":0.0},{"x":-0.5,"y":0.0},{"x":-0.25,"y":0.0},{"x":0.0,"y":0.0},{"x":0.25,"y":0.0},{"x":0.5,"y":8.0E-4},{"x":0.75,"y":0.02},{"x":1.0,"y":0.0748},{"x":1.25,"y":0.1388},{"x":1.5,"y":0.2272},{"x":1.75,"y":0.294},{"x":2.0,"y":0.3168},{"x":2.25,"y":0.3368},{"x":2.5,"y":0.3244},{"x":2.75,"y":0.3048},{"x":3.0,"y":0.2652},{"x":3.25,"y":0.2472},{"x":3.5,"y":0.212},{"x":3.75,"y":0.1896},{"x":4.0,"y":0.1696},{"x":4.25,"y":0.1436},{"x":4.5,"y":0.1136},{"x":4.75,"y":0.0964},{"x":5.0,"y":0.0876},{"x":5.25,"y":0.0736},{"x":5.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"ace83c27-5118-4297-a23f-db64e1b92902"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"8f414a15-34b9-4928-882d-a347fef58e78"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-5 5]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"ace83c27-5118-4297-a23f-db64e1b92902\", :values ({:x -5.0, :y 0} {:x -4.875, :y 0.0} {:x -4.75, :y 0.0} {:x -4.625, :y 0.0} {:x -4.5, :y 0.0} {:x -4.375, :y 0.0} {:x -4.25, :y 0.0} {:x -4.125, :y 0.0} {:x -4.0, :y 0.0} {:x -3.875, :y 0.0} {:x -3.75, :y 0.0} {:x -3.625, :y 0.0} {:x -3.5, :y 0.0} {:x -3.375, :y 0.0} {:x -3.25, :y 0.0} {:x -3.125, :y 0.0} {:x -3.0, :y 0.0} {:x -2.875, :y 0.0} {:x -2.75, :y 0.0} {:x -2.625, :y 0.0} {:x -2.5, :y 0.0} {:x -2.375, :y 0.0} {:x -2.25, :y 0.0} {:x -2.125, :y 0.0} {:x -2.0, :y 0.0} {:x -1.875, :y 0.0} {:x -1.75, :y 0.0} {:x -1.625, :y 0.0} {:x -1.5, :y 0.0} {:x -1.375, :y 0.0} {:x -1.25, :y 0.0} {:x -1.125, :y 0.0} {:x -1.0, :y 8.0E-4} {:x -0.875, :y 0.0016} {:x -0.75, :y 0.0016} {:x -0.625, :y 0.0024} {:x -0.5, :y 0.008} {:x -0.375, :y 0.0096} {:x -0.25, :y 0.0264} {:x -0.125, :y 0.048} {:x 0.0, :y 0.084} {:x 0.125, :y 0.1424} {:x 0.25, :y 0.216} {:x 0.375, :y 0.3152} {:x 0.5, :y 0.4088} {:x 0.625, :y 0.564} {:x 0.75, :y 0.6376} {:x 0.875, :y 0.7832} {:x 1.0, :y 0.82} {:x 1.125, :y 0.78} {:x 1.25, :y 0.6928} {:x 1.375, :y 0.6344} {:x 1.5, :y 0.5528} {:x 1.625, :y 0.4048} {:x 1.75, :y 0.3024} {:x 1.875, :y 0.2336} {:x 2.0, :y 0.1504} {:x 2.125, :y 0.0936} {:x 2.25, :y 0.0424} {:x 2.375, :y 0.0208} {:x 2.5, :y 0.0088} {:x 2.625, :y 0.0056} {:x 2.75, :y 0.0072} {:x 2.875, :y 0.0} {:x 3.0, :y 8.0E-4} {:x 3.125, :y 0.0} {:x 3.25, :y 0.0} {:x 3.375, :y 0.0} {:x 3.5, :y 0.0} {:x 3.625, :y 0.0} {:x 3.75, :y 0.0} {:x 3.875, :y 0.0} {:x 4.0, :y 0.0} {:x 4.125, :y 0.0} {:x 4.25, :y 0.0} {:x 4.375, :y 0.0} {:x 4.5, :y 0.0} {:x 4.625, :y 0.0} {:x 4.75, :y 0.0} {:x 4.875, :y 0.0} {:x 5.0, :y 0.0} {:x 5.125, :y 0.0} {:x 5.25, :y 0})} {:name \"8f414a15-34b9-4928-882d-a347fef58e78\", :values ({:x -5.0, :y 0} {:x -4.75, :y 0.0} {:x -4.5, :y 0.0} {:x -4.25, :y 0.0} {:x -4.0, :y 0.0} {:x -3.75, :y 0.0} {:x -3.5, :y 0.0} {:x -3.25, :y 0.0} {:x -3.0, :y 0.0} {:x -2.75, :y 0.0} {:x -2.5, :y 0.0} {:x -2.25, :y 0.0} {:x -2.0, :y 0.0} {:x -1.75, :y 0.0} {:x -1.5, :y 0.0} {:x -1.25, :y 0.0} {:x -1.0, :y 0.0} {:x -0.75, :y 0.0} {:x -0.5, :y 0.0} {:x -0.25, :y 0.0} {:x 0.0, :y 0.0} {:x 0.25, :y 0.0} {:x 0.5, :y 8.0E-4} {:x 0.75, :y 0.02} {:x 1.0, :y 0.0748} {:x 1.25, :y 0.1388} {:x 1.5, :y 0.2272} {:x 1.75, :y 0.294} {:x 2.0, :y 0.3168} {:x 2.25, :y 0.3368} {:x 2.5, :y 0.3244} {:x 2.75, :y 0.3048} {:x 3.0, :y 0.2652} {:x 3.25, :y 0.2472} {:x 3.5, :y 0.212} {:x 3.75, :y 0.1896} {:x 4.0, :y 0.1696} {:x 4.25, :y 0.1436} {:x 4.5, :y 0.1136} {:x 4.75, :y 0.0964} {:x 5.0, :y 0.0876} {:x 5.25, :y 0.0736} {:x 5.5, :y 0})}), :marks ({:type \"line\", :from {:data \"ace83c27-5118-4297-a23f-db64e1b92902\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"8f414a15-34b9-4928-882d-a347fef58e78\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@
(defdist dirac
  "Dirac distribution"
  [x]  ; distribution parameters
  []   ; auxiliary bindings
  (sample* [this] x)
  (observe* [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@b37528&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@b37528>"}
;; <=

;; @@
(defm div-increase-distrib [Ft]
  ; takes F(t-1) spike (for example, prev. september)
  ; returns boolean
  (if (= (:spike Ft) 1) ; spike in september
      (sample (flip 0.9)) ; high prob. of increase
      (sample (flip 0.1)))) ; low prob.
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-increase-distrib</span>","value":"#'financial-forecasting/div-increase-distrib"}
;; <=

;; @@
(defm amount-to-increase-div-distrib [dt]
  (if dt ; decision to increase or not to increase dividents
    (sample (uniform-continuous 0.05 0.10))
    (0.0)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/amount-to-increase-div-distrib</span>","value":"#'financial-forecasting/amount-to-increase-div-distrib"}
;; <=

;; @@
(with-primitive-procedures [log-normal]
  (defquery at-given-dt-Ft [Ft] ; this is "previous" Ft
    ; This is a model (simulator) for amount of dividents for next quarter

    ; TODO: get addition _value_
    (let [ addition-distrib
             (amount-to-increase-div-distrib (sample (div-increase-distrib Ft)))
           addition (sample addition-distrib)]
         (print (type addition))
         (+ (:div Ft) addition))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/at-given-dt-Ft</span>","value":"#'financial-forecasting/at-given-dt-Ft"}
;; <=

;; @@
;(def Ft-data-0 (get Ft-data :'30-06-2013'))
(def Ft-data-0 (get Ft-data :'31-03-2014'))

(println Ft-data-0)

(->> (doquery :smc
  at-given-dt-Ft [Ft-data-0] ; feed previous frame (Ft) here
  :number-of-particles 100)
     (take 5000)
     stat/collect-results ; TODO: () ??
     (stat/empirical-mean))

;; @@
;; ->
;;; {:stock-price 70.97, :volume 50194333, :div 0.6, :net-income 0.65, :spike 0}
;;;
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
