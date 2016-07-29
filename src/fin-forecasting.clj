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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@bd9fc3&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@bd9fc3>"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-5,5]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"19b98071-363f-424b-8b9c-76c1a616cf7e","values":[{"x":-5.0,"y":0},{"x":-4.875,"y":0.0},{"x":-4.75,"y":0.0},{"x":-4.625,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.375,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.125,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.875,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.625,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.375,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.125,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.875,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.625,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.375,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.125,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.875,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.625,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.375,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.125,"y":8.0E-4},{"x":-1.0,"y":8.0E-4},{"x":-0.875,"y":8.0E-4},{"x":-0.75,"y":0.0016},{"x":-0.625,"y":8.0E-4},{"x":-0.5,"y":0.0048},{"x":-0.375,"y":0.0128},{"x":-0.25,"y":0.0192},{"x":-0.125,"y":0.0432},{"x":0.0,"y":0.0824},{"x":0.125,"y":0.1392},{"x":0.25,"y":0.212},{"x":0.375,"y":0.3208},{"x":0.5,"y":0.4288},{"x":0.625,"y":0.5192},{"x":0.75,"y":0.6432},{"x":0.875,"y":0.7456},{"x":1.0,"y":0.7872},{"x":1.125,"y":0.7944},{"x":1.25,"y":0.7472},{"x":1.375,"y":0.6768},{"x":1.5,"y":0.5496},{"x":1.625,"y":0.408},{"x":1.75,"y":0.3312},{"x":1.875,"y":0.1976},{"x":2.0,"y":0.1376},{"x":2.125,"y":0.0904},{"x":2.25,"y":0.0504},{"x":2.375,"y":0.0304},{"x":2.5,"y":0.0112},{"x":2.625,"y":0.0072},{"x":2.75,"y":0.0032},{"x":2.875,"y":8.0E-4},{"x":3.0,"y":8.0E-4},{"x":3.125,"y":0.0},{"x":3.25,"y":0.0},{"x":3.375,"y":0.0},{"x":3.5,"y":0.0},{"x":3.625,"y":0.0},{"x":3.75,"y":0.0},{"x":3.875,"y":0.0},{"x":4.0,"y":0.0},{"x":4.125,"y":0.0},{"x":4.25,"y":0.0},{"x":4.375,"y":0.0},{"x":4.5,"y":0.0},{"x":4.625,"y":0.0},{"x":4.75,"y":0.0},{"x":4.875,"y":0.0},{"x":5.0,"y":0.0},{"x":5.125,"y":0.0},{"x":5.25,"y":0}]},{"name":"40f0c24f-bd3e-4f25-94ae-e6b53c172170","values":[{"x":-5.0,"y":0},{"x":-4.75,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.75,"y":0.0},{"x":-0.5,"y":0.0},{"x":-0.25,"y":0.0},{"x":0.0,"y":0.0},{"x":0.25,"y":0.0},{"x":0.5,"y":0.0016},{"x":0.75,"y":0.0164},{"x":1.0,"y":0.0728},{"x":1.25,"y":0.1444},{"x":1.5,"y":0.2292},{"x":1.75,"y":0.2864},{"x":2.0,"y":0.3132},{"x":2.25,"y":0.3132},{"x":2.5,"y":0.3068},{"x":2.75,"y":0.3088},{"x":3.0,"y":0.2832},{"x":3.25,"y":0.2512},{"x":3.5,"y":0.208},{"x":3.75,"y":0.1992},{"x":4.0,"y":0.16},{"x":4.25,"y":0.1416},{"x":4.5,"y":0.1188},{"x":4.75,"y":0.1024},{"x":5.0,"y":0.0832},{"x":5.25,"y":0.0696},{"x":5.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"19b98071-363f-424b-8b9c-76c1a616cf7e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"40f0c24f-bd3e-4f25-94ae-e6b53c172170"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-5 5]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"19b98071-363f-424b-8b9c-76c1a616cf7e\", :values ({:x -5.0, :y 0} {:x -4.875, :y 0.0} {:x -4.75, :y 0.0} {:x -4.625, :y 0.0} {:x -4.5, :y 0.0} {:x -4.375, :y 0.0} {:x -4.25, :y 0.0} {:x -4.125, :y 0.0} {:x -4.0, :y 0.0} {:x -3.875, :y 0.0} {:x -3.75, :y 0.0} {:x -3.625, :y 0.0} {:x -3.5, :y 0.0} {:x -3.375, :y 0.0} {:x -3.25, :y 0.0} {:x -3.125, :y 0.0} {:x -3.0, :y 0.0} {:x -2.875, :y 0.0} {:x -2.75, :y 0.0} {:x -2.625, :y 0.0} {:x -2.5, :y 0.0} {:x -2.375, :y 0.0} {:x -2.25, :y 0.0} {:x -2.125, :y 0.0} {:x -2.0, :y 0.0} {:x -1.875, :y 0.0} {:x -1.75, :y 0.0} {:x -1.625, :y 0.0} {:x -1.5, :y 0.0} {:x -1.375, :y 0.0} {:x -1.25, :y 0.0} {:x -1.125, :y 8.0E-4} {:x -1.0, :y 8.0E-4} {:x -0.875, :y 8.0E-4} {:x -0.75, :y 0.0016} {:x -0.625, :y 8.0E-4} {:x -0.5, :y 0.0048} {:x -0.375, :y 0.0128} {:x -0.25, :y 0.0192} {:x -0.125, :y 0.0432} {:x 0.0, :y 0.0824} {:x 0.125, :y 0.1392} {:x 0.25, :y 0.212} {:x 0.375, :y 0.3208} {:x 0.5, :y 0.4288} {:x 0.625, :y 0.5192} {:x 0.75, :y 0.6432} {:x 0.875, :y 0.7456} {:x 1.0, :y 0.7872} {:x 1.125, :y 0.7944} {:x 1.25, :y 0.7472} {:x 1.375, :y 0.6768} {:x 1.5, :y 0.5496} {:x 1.625, :y 0.408} {:x 1.75, :y 0.3312} {:x 1.875, :y 0.1976} {:x 2.0, :y 0.1376} {:x 2.125, :y 0.0904} {:x 2.25, :y 0.0504} {:x 2.375, :y 0.0304} {:x 2.5, :y 0.0112} {:x 2.625, :y 0.0072} {:x 2.75, :y 0.0032} {:x 2.875, :y 8.0E-4} {:x 3.0, :y 8.0E-4} {:x 3.125, :y 0.0} {:x 3.25, :y 0.0} {:x 3.375, :y 0.0} {:x 3.5, :y 0.0} {:x 3.625, :y 0.0} {:x 3.75, :y 0.0} {:x 3.875, :y 0.0} {:x 4.0, :y 0.0} {:x 4.125, :y 0.0} {:x 4.25, :y 0.0} {:x 4.375, :y 0.0} {:x 4.5, :y 0.0} {:x 4.625, :y 0.0} {:x 4.75, :y 0.0} {:x 4.875, :y 0.0} {:x 5.0, :y 0.0} {:x 5.125, :y 0.0} {:x 5.25, :y 0})} {:name \"40f0c24f-bd3e-4f25-94ae-e6b53c172170\", :values ({:x -5.0, :y 0} {:x -4.75, :y 0.0} {:x -4.5, :y 0.0} {:x -4.25, :y 0.0} {:x -4.0, :y 0.0} {:x -3.75, :y 0.0} {:x -3.5, :y 0.0} {:x -3.25, :y 0.0} {:x -3.0, :y 0.0} {:x -2.75, :y 0.0} {:x -2.5, :y 0.0} {:x -2.25, :y 0.0} {:x -2.0, :y 0.0} {:x -1.75, :y 0.0} {:x -1.5, :y 0.0} {:x -1.25, :y 0.0} {:x -1.0, :y 0.0} {:x -0.75, :y 0.0} {:x -0.5, :y 0.0} {:x -0.25, :y 0.0} {:x 0.0, :y 0.0} {:x 0.25, :y 0.0} {:x 0.5, :y 0.0016} {:x 0.75, :y 0.0164} {:x 1.0, :y 0.0728} {:x 1.25, :y 0.1444} {:x 1.5, :y 0.2292} {:x 1.75, :y 0.2864} {:x 2.0, :y 0.3132} {:x 2.25, :y 0.3132} {:x 2.5, :y 0.3068} {:x 2.75, :y 0.3088} {:x 3.0, :y 0.2832} {:x 3.25, :y 0.2512} {:x 3.5, :y 0.208} {:x 3.75, :y 0.1992} {:x 4.0, :y 0.16} {:x 4.25, :y 0.1416} {:x 4.5, :y 0.1188} {:x 4.75, :y 0.1024} {:x 5.0, :y 0.0832} {:x 5.25, :y 0.0696} {:x 5.5, :y 0})}), :marks ({:type \"line\", :from {:data \"19b98071-363f-424b-8b9c-76c1a616cf7e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"40f0c24f-bd3e-4f25-94ae-e6b53c172170\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@bd9fc3&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@bd9fc3>"}
;; <=

;; @@
; TODO: take F(t-1) september for spike prop.
(defm div-increase-distrib [Ft]                                  ; returns boolean
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

    ; amount-to-increase-div-distrib ( div-increase-distrib )

      (let [addition ; TODO: get addition _value_
             (sample
               (amount-to-increase-div-distrib
                 (sample (div-increase-distrib Ft))))]
           (print addition)
        (+ (:div Ft) ;(observe addition)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/at-given-dt-Ft</span>","value":"#'financial-forecasting/at-given-dt-Ft"}
;; <=

;; @@
;(def Ft-data-0 (get Ft-data :'30-06-2013'))
(def Ft-data-0 (get Ft-data :'31-03-2014'))

(print Ft-data-0)

(->> (doquery :smc
  at-given-dt-Ft [Ft-data-0] ; feed previous frame (Ft) here
  :number-of-particles 100)
     (take 5000)
     stat/collect-results
     (stat/empirical-mean))

;; @@
;; ->
;;; {:stock-price 70.97, :volume 50194333, :div 0.6, :net-income 0.65, :spike 0}
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@

;; @@

;; @@
