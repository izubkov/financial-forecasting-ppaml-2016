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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@1c4344c&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@1c4344c>"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-5,5]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"d1c7fbb7-2e14-4175-811f-b4a91faf4b9e","values":[{"x":-5.0,"y":0},{"x":-4.875,"y":0.0},{"x":-4.75,"y":0.0},{"x":-4.625,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.375,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.125,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.875,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.625,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.375,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.125,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.875,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.625,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.375,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.125,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.875,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.625,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.375,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.125,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.875,"y":8.0E-4},{"x":-0.75,"y":8.0E-4},{"x":-0.625,"y":0.0024},{"x":-0.5,"y":0.0048},{"x":-0.375,"y":0.0112},{"x":-0.25,"y":0.024},{"x":-0.125,"y":0.0544},{"x":0.0,"y":0.0888},{"x":0.125,"y":0.14},{"x":0.25,"y":0.2008},{"x":0.375,"y":0.3056},{"x":0.5,"y":0.4048},{"x":0.625,"y":0.572},{"x":0.75,"y":0.6768},{"x":0.875,"y":0.7344},{"x":1.0,"y":0.8144},{"x":1.125,"y":0.7992},{"x":1.25,"y":0.7632},{"x":1.375,"y":0.588},{"x":1.5,"y":0.5336},{"x":1.625,"y":0.4232},{"x":1.75,"y":0.2992},{"x":1.875,"y":0.2248},{"x":2.0,"y":0.1448},{"x":2.125,"y":0.0872},{"x":2.25,"y":0.0536},{"x":2.375,"y":0.0256},{"x":2.5,"y":0.0104},{"x":2.625,"y":0.0064},{"x":2.75,"y":0.0024},{"x":2.875,"y":0.0024},{"x":3.0,"y":0.0},{"x":3.125,"y":0.0},{"x":3.25,"y":0.0},{"x":3.375,"y":0.0},{"x":3.5,"y":0.0},{"x":3.625,"y":0.0},{"x":3.75,"y":0.0},{"x":3.875,"y":0.0},{"x":4.0,"y":0.0},{"x":4.125,"y":0.0},{"x":4.25,"y":0.0},{"x":4.375,"y":0.0},{"x":4.5,"y":0.0},{"x":4.625,"y":0.0},{"x":4.75,"y":0.0},{"x":4.875,"y":0.0},{"x":5.0,"y":0.0},{"x":5.125,"y":0.0},{"x":5.25,"y":0}]},{"name":"4dc2edd1-51e6-473a-b041-8f5383cc4813","values":[{"x":-5.0,"y":0},{"x":-4.75,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.75,"y":0.0},{"x":-0.5,"y":0.0},{"x":-0.25,"y":0.0},{"x":0.0,"y":0.0},{"x":0.25,"y":0.0},{"x":0.5,"y":0.0012},{"x":0.75,"y":0.0216},{"x":1.0,"y":0.078},{"x":1.25,"y":0.1464},{"x":1.5,"y":0.2368},{"x":1.75,"y":0.2916},{"x":2.0,"y":0.322},{"x":2.25,"y":0.3248},{"x":2.5,"y":0.3112},{"x":2.75,"y":0.3092},{"x":3.0,"y":0.2952},{"x":3.25,"y":0.2548},{"x":3.5,"y":0.204},{"x":3.75,"y":0.1784},{"x":4.0,"y":0.1604},{"x":4.25,"y":0.132},{"x":4.5,"y":0.1104},{"x":4.75,"y":0.1012},{"x":5.0,"y":0.0752},{"x":5.25,"y":0.0812},{"x":5.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"d1c7fbb7-2e14-4175-811f-b4a91faf4b9e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"4dc2edd1-51e6-473a-b041-8f5383cc4813"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-5 5]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"d1c7fbb7-2e14-4175-811f-b4a91faf4b9e\", :values ({:x -5.0, :y 0} {:x -4.875, :y 0.0} {:x -4.75, :y 0.0} {:x -4.625, :y 0.0} {:x -4.5, :y 0.0} {:x -4.375, :y 0.0} {:x -4.25, :y 0.0} {:x -4.125, :y 0.0} {:x -4.0, :y 0.0} {:x -3.875, :y 0.0} {:x -3.75, :y 0.0} {:x -3.625, :y 0.0} {:x -3.5, :y 0.0} {:x -3.375, :y 0.0} {:x -3.25, :y 0.0} {:x -3.125, :y 0.0} {:x -3.0, :y 0.0} {:x -2.875, :y 0.0} {:x -2.75, :y 0.0} {:x -2.625, :y 0.0} {:x -2.5, :y 0.0} {:x -2.375, :y 0.0} {:x -2.25, :y 0.0} {:x -2.125, :y 0.0} {:x -2.0, :y 0.0} {:x -1.875, :y 0.0} {:x -1.75, :y 0.0} {:x -1.625, :y 0.0} {:x -1.5, :y 0.0} {:x -1.375, :y 0.0} {:x -1.25, :y 0.0} {:x -1.125, :y 0.0} {:x -1.0, :y 0.0} {:x -0.875, :y 8.0E-4} {:x -0.75, :y 8.0E-4} {:x -0.625, :y 0.0024} {:x -0.5, :y 0.0048} {:x -0.375, :y 0.0112} {:x -0.25, :y 0.024} {:x -0.125, :y 0.0544} {:x 0.0, :y 0.0888} {:x 0.125, :y 0.14} {:x 0.25, :y 0.2008} {:x 0.375, :y 0.3056} {:x 0.5, :y 0.4048} {:x 0.625, :y 0.572} {:x 0.75, :y 0.6768} {:x 0.875, :y 0.7344} {:x 1.0, :y 0.8144} {:x 1.125, :y 0.7992} {:x 1.25, :y 0.7632} {:x 1.375, :y 0.588} {:x 1.5, :y 0.5336} {:x 1.625, :y 0.4232} {:x 1.75, :y 0.2992} {:x 1.875, :y 0.2248} {:x 2.0, :y 0.1448} {:x 2.125, :y 0.0872} {:x 2.25, :y 0.0536} {:x 2.375, :y 0.0256} {:x 2.5, :y 0.0104} {:x 2.625, :y 0.0064} {:x 2.75, :y 0.0024} {:x 2.875, :y 0.0024} {:x 3.0, :y 0.0} {:x 3.125, :y 0.0} {:x 3.25, :y 0.0} {:x 3.375, :y 0.0} {:x 3.5, :y 0.0} {:x 3.625, :y 0.0} {:x 3.75, :y 0.0} {:x 3.875, :y 0.0} {:x 4.0, :y 0.0} {:x 4.125, :y 0.0} {:x 4.25, :y 0.0} {:x 4.375, :y 0.0} {:x 4.5, :y 0.0} {:x 4.625, :y 0.0} {:x 4.75, :y 0.0} {:x 4.875, :y 0.0} {:x 5.0, :y 0.0} {:x 5.125, :y 0.0} {:x 5.25, :y 0})} {:name \"4dc2edd1-51e6-473a-b041-8f5383cc4813\", :values ({:x -5.0, :y 0} {:x -4.75, :y 0.0} {:x -4.5, :y 0.0} {:x -4.25, :y 0.0} {:x -4.0, :y 0.0} {:x -3.75, :y 0.0} {:x -3.5, :y 0.0} {:x -3.25, :y 0.0} {:x -3.0, :y 0.0} {:x -2.75, :y 0.0} {:x -2.5, :y 0.0} {:x -2.25, :y 0.0} {:x -2.0, :y 0.0} {:x -1.75, :y 0.0} {:x -1.5, :y 0.0} {:x -1.25, :y 0.0} {:x -1.0, :y 0.0} {:x -0.75, :y 0.0} {:x -0.5, :y 0.0} {:x -0.25, :y 0.0} {:x 0.0, :y 0.0} {:x 0.25, :y 0.0} {:x 0.5, :y 0.0012} {:x 0.75, :y 0.0216} {:x 1.0, :y 0.078} {:x 1.25, :y 0.1464} {:x 1.5, :y 0.2368} {:x 1.75, :y 0.2916} {:x 2.0, :y 0.322} {:x 2.25, :y 0.3248} {:x 2.5, :y 0.3112} {:x 2.75, :y 0.3092} {:x 3.0, :y 0.2952} {:x 3.25, :y 0.2548} {:x 3.5, :y 0.204} {:x 3.75, :y 0.1784} {:x 4.0, :y 0.1604} {:x 4.25, :y 0.132} {:x 4.5, :y 0.1104} {:x 4.75, :y 0.1012} {:x 5.0, :y 0.0752} {:x 5.25, :y 0.0812} {:x 5.5, :y 0})}), :marks ({:type \"line\", :from {:data \"d1c7fbb7-2e14-4175-811f-b4a91faf4b9e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"4dc2edd1-51e6-473a-b041-8f5383cc4813\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@1c4344c&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@1c4344c>"}
;; <=

;; @@
(defm div-percent-distrib [Ft]
  (if 
    (if (> (:volume Ft) 0.5)
      (sample (flip 0.9))
      (sample (flip 0.1)))
    1
    0))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-percent-distrib</span>","value":"#'financial-forecasting/div-percent-distrib"}
;; <=

;; @@
(with-primitive-procedures [log-normal]
  (defquery at-given-dt-Ft [Ft dt]
    (let []
      (sample 
        (if dt
          (let [div-percent (div-percent-distrib Ft)]
            (log-normal
              (* div-percent (:stock-price Ft)) 1))
          (dirac 0))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/at-given-dt-Ft</span>","value":"#'financial-forecasting/at-given-dt-Ft"}
;; <=

;; @@
(def Ft-data-0 (get Ft-data :'30-06-2013'))

(print Ft-data-0)

(->> (doquery :smc
  at-given-dt-Ft [Ft-data-0 true]
  ;at-given-dt-Ft [Ft-data-0]
  :number-of-particles 100)
     (take 5000)
     stat/collect-results
     (stat/empirical-mean))

;; @@
;; ->
;;; {:stock-price 58.35, :volume 60551066, :div 0.5625, :net-income 0.35}
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-double'>3.21774473082266E25</span>","value":"3.21774473082266E25"}
;; <=

;; @@

;; @@

;; @@

;; @@
