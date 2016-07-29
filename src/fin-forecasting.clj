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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@1b18bb3&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@1b18bb3>"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-5,5]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"fd4003e8-9e1a-42e1-99bb-beba322befb1","values":[{"x":-5.0,"y":0},{"x":-4.875,"y":0.0},{"x":-4.75,"y":0.0},{"x":-4.625,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.375,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.125,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.875,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.625,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.375,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.125,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.875,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.625,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.375,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.125,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.875,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.625,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.375,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.125,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.875,"y":8.0E-4},{"x":-0.75,"y":8.0E-4},{"x":-0.625,"y":0.0032},{"x":-0.5,"y":0.0032},{"x":-0.375,"y":0.0096},{"x":-0.25,"y":0.0232},{"x":-0.125,"y":0.0504},{"x":0.0,"y":0.0944},{"x":0.125,"y":0.128},{"x":0.25,"y":0.2104},{"x":0.375,"y":0.3512},{"x":0.5,"y":0.4152},{"x":0.625,"y":0.54},{"x":0.75,"y":0.6768},{"x":0.875,"y":0.7544},{"x":1.0,"y":0.8144},{"x":1.125,"y":0.7536},{"x":1.25,"y":0.7408},{"x":1.375,"y":0.6296},{"x":1.5,"y":0.5568},{"x":1.625,"y":0.4192},{"x":1.75,"y":0.296},{"x":1.875,"y":0.2152},{"x":2.0,"y":0.132},{"x":2.125,"y":0.0816},{"x":2.25,"y":0.0456},{"x":2.375,"y":0.0272},{"x":2.5,"y":0.0184},{"x":2.625,"y":0.0048},{"x":2.75,"y":8.0E-4},{"x":2.875,"y":0.0016},{"x":3.0,"y":8.0E-4},{"x":3.125,"y":0.0},{"x":3.25,"y":0.0},{"x":3.375,"y":0.0},{"x":3.5,"y":0.0},{"x":3.625,"y":0.0},{"x":3.75,"y":0.0},{"x":3.875,"y":0.0},{"x":4.0,"y":0.0},{"x":4.125,"y":0.0},{"x":4.25,"y":0.0},{"x":4.375,"y":0.0},{"x":4.5,"y":0.0},{"x":4.625,"y":0.0},{"x":4.75,"y":0.0},{"x":4.875,"y":0.0},{"x":5.0,"y":0.0},{"x":5.125,"y":0.0},{"x":5.25,"y":0}]},{"name":"2fe635f2-92ed-43ad-9c74-4f7fdaf89ed1","values":[{"x":-5.0,"y":0},{"x":-4.75,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.75,"y":0.0},{"x":-0.5,"y":0.0},{"x":-0.25,"y":0.0},{"x":0.0,"y":0.0},{"x":0.25,"y":0.0},{"x":0.5,"y":8.0E-4},{"x":0.75,"y":0.0192},{"x":1.0,"y":0.068},{"x":1.25,"y":0.1316},{"x":1.5,"y":0.23},{"x":1.75,"y":0.2804},{"x":2.0,"y":0.3308},{"x":2.25,"y":0.3508},{"x":2.5,"y":0.3224},{"x":2.75,"y":0.296},{"x":3.0,"y":0.2892},{"x":3.25,"y":0.2556},{"x":3.5,"y":0.208},{"x":3.75,"y":0.1832},{"x":4.0,"y":0.148},{"x":4.25,"y":0.1292},{"x":4.5,"y":0.12},{"x":4.75,"y":0.104},{"x":5.0,"y":0.0732},{"x":5.25,"y":0.0664},{"x":5.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"fd4003e8-9e1a-42e1-99bb-beba322befb1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"2fe635f2-92ed-43ad-9c74-4f7fdaf89ed1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-5 5]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"fd4003e8-9e1a-42e1-99bb-beba322befb1\", :values ({:x -5.0, :y 0} {:x -4.875, :y 0.0} {:x -4.75, :y 0.0} {:x -4.625, :y 0.0} {:x -4.5, :y 0.0} {:x -4.375, :y 0.0} {:x -4.25, :y 0.0} {:x -4.125, :y 0.0} {:x -4.0, :y 0.0} {:x -3.875, :y 0.0} {:x -3.75, :y 0.0} {:x -3.625, :y 0.0} {:x -3.5, :y 0.0} {:x -3.375, :y 0.0} {:x -3.25, :y 0.0} {:x -3.125, :y 0.0} {:x -3.0, :y 0.0} {:x -2.875, :y 0.0} {:x -2.75, :y 0.0} {:x -2.625, :y 0.0} {:x -2.5, :y 0.0} {:x -2.375, :y 0.0} {:x -2.25, :y 0.0} {:x -2.125, :y 0.0} {:x -2.0, :y 0.0} {:x -1.875, :y 0.0} {:x -1.75, :y 0.0} {:x -1.625, :y 0.0} {:x -1.5, :y 0.0} {:x -1.375, :y 0.0} {:x -1.25, :y 0.0} {:x -1.125, :y 0.0} {:x -1.0, :y 0.0} {:x -0.875, :y 8.0E-4} {:x -0.75, :y 8.0E-4} {:x -0.625, :y 0.0032} {:x -0.5, :y 0.0032} {:x -0.375, :y 0.0096} {:x -0.25, :y 0.0232} {:x -0.125, :y 0.0504} {:x 0.0, :y 0.0944} {:x 0.125, :y 0.128} {:x 0.25, :y 0.2104} {:x 0.375, :y 0.3512} {:x 0.5, :y 0.4152} {:x 0.625, :y 0.54} {:x 0.75, :y 0.6768} {:x 0.875, :y 0.7544} {:x 1.0, :y 0.8144} {:x 1.125, :y 0.7536} {:x 1.25, :y 0.7408} {:x 1.375, :y 0.6296} {:x 1.5, :y 0.5568} {:x 1.625, :y 0.4192} {:x 1.75, :y 0.296} {:x 1.875, :y 0.2152} {:x 2.0, :y 0.132} {:x 2.125, :y 0.0816} {:x 2.25, :y 0.0456} {:x 2.375, :y 0.0272} {:x 2.5, :y 0.0184} {:x 2.625, :y 0.0048} {:x 2.75, :y 8.0E-4} {:x 2.875, :y 0.0016} {:x 3.0, :y 8.0E-4} {:x 3.125, :y 0.0} {:x 3.25, :y 0.0} {:x 3.375, :y 0.0} {:x 3.5, :y 0.0} {:x 3.625, :y 0.0} {:x 3.75, :y 0.0} {:x 3.875, :y 0.0} {:x 4.0, :y 0.0} {:x 4.125, :y 0.0} {:x 4.25, :y 0.0} {:x 4.375, :y 0.0} {:x 4.5, :y 0.0} {:x 4.625, :y 0.0} {:x 4.75, :y 0.0} {:x 4.875, :y 0.0} {:x 5.0, :y 0.0} {:x 5.125, :y 0.0} {:x 5.25, :y 0})} {:name \"2fe635f2-92ed-43ad-9c74-4f7fdaf89ed1\", :values ({:x -5.0, :y 0} {:x -4.75, :y 0.0} {:x -4.5, :y 0.0} {:x -4.25, :y 0.0} {:x -4.0, :y 0.0} {:x -3.75, :y 0.0} {:x -3.5, :y 0.0} {:x -3.25, :y 0.0} {:x -3.0, :y 0.0} {:x -2.75, :y 0.0} {:x -2.5, :y 0.0} {:x -2.25, :y 0.0} {:x -2.0, :y 0.0} {:x -1.75, :y 0.0} {:x -1.5, :y 0.0} {:x -1.25, :y 0.0} {:x -1.0, :y 0.0} {:x -0.75, :y 0.0} {:x -0.5, :y 0.0} {:x -0.25, :y 0.0} {:x 0.0, :y 0.0} {:x 0.25, :y 0.0} {:x 0.5, :y 8.0E-4} {:x 0.75, :y 0.0192} {:x 1.0, :y 0.068} {:x 1.25, :y 0.1316} {:x 1.5, :y 0.23} {:x 1.75, :y 0.2804} {:x 2.0, :y 0.3308} {:x 2.25, :y 0.3508} {:x 2.5, :y 0.3224} {:x 2.75, :y 0.296} {:x 3.0, :y 0.2892} {:x 3.25, :y 0.2556} {:x 3.5, :y 0.208} {:x 3.75, :y 0.1832} {:x 4.0, :y 0.148} {:x 4.25, :y 0.1292} {:x 4.5, :y 0.12} {:x 4.75, :y 0.104} {:x 5.0, :y 0.0732} {:x 5.25, :y 0.0664} {:x 5.5, :y 0})}), :marks ({:type \"line\", :from {:data \"fd4003e8-9e1a-42e1-99bb-beba322befb1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"2fe635f2-92ed-43ad-9c74-4f7fdaf89ed1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@1b18bb3&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@1b18bb3>"}
;; <=

;; @@
(defm div-increase-boolean [Ft] ; Dt
  ; takes F(t-1) spike (for example, prev. september)
  ; returns boolean
  (if (= (:spike Ft) 1) ; spike in september
      (sample (flip 0.9)) ; high prob. of increase
      (sample (flip 0.1)))) ; low prob.
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-increase-boolean</span>","value":"#'financial-forecasting/div-increase-boolean"}
;; <=

;; @@
(defm amount-to-increase-div-distrib [dt]
  (if dt ; decision to increase or not to increase dividents
    (uniform-continuous 0.05 0.10)
    (uniform-continuous 0.00 0.00000000001)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/amount-to-increase-div-distrib</span>","value":"#'financial-forecasting/amount-to-increase-div-distrib"}
;; <=

;; @@
(defquery at-given-dt-Ft [Ft] ; this is "previous" Ft
  ; This is a model (simulator) for amount of dividents for next quarter

  ; TODO: get addition _value_
  (let [addition-distrib
           (amount-to-increase-div-distrib (div-increase-boolean Ft))]
       (sample addition-distrib)))
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
;;; {"type":"html","content":"<span class='clj-double'>0.007216763696552145</span>","value":"0.007216763696552145"}
;; <=

;; @@

;; @@
