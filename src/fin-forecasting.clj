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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-5,5]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"f177f054-0598-4b8b-a288-54cf303c1ad6","values":[{"x":-5.0,"y":0},{"x":-4.875,"y":0.0},{"x":-4.75,"y":0.0},{"x":-4.625,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.375,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.125,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.875,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.625,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.375,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.125,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.875,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.625,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.375,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.125,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.875,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.625,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.375,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.125,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.875,"y":8.0E-4},{"x":-0.75,"y":0.0016},{"x":-0.625,"y":0.004},{"x":-0.5,"y":0.0064},{"x":-0.375,"y":0.0152},{"x":-0.25,"y":0.0272},{"x":-0.125,"y":0.0536},{"x":0.0,"y":0.088},{"x":0.125,"y":0.1408},{"x":0.25,"y":0.2344},{"x":0.375,"y":0.3424},{"x":0.5,"y":0.4152},{"x":0.625,"y":0.492},{"x":0.75,"y":0.6712},{"x":0.875,"y":0.7784},{"x":1.0,"y":0.7792},{"x":1.125,"y":0.8536},{"x":1.25,"y":0.716},{"x":1.375,"y":0.616},{"x":1.5,"y":0.5344},{"x":1.625,"y":0.4224},{"x":1.75,"y":0.3112},{"x":1.875,"y":0.1928},{"x":2.0,"y":0.1248},{"x":2.125,"y":0.0808},{"x":2.25,"y":0.044},{"x":2.375,"y":0.0344},{"x":2.5,"y":0.0128},{"x":2.625,"y":0.0032},{"x":2.75,"y":0.0032},{"x":2.875,"y":0.0},{"x":3.0,"y":0.0},{"x":3.125,"y":0.0},{"x":3.25,"y":0.0},{"x":3.375,"y":0.0},{"x":3.5,"y":0.0},{"x":3.625,"y":0.0},{"x":3.75,"y":0.0},{"x":3.875,"y":0.0},{"x":4.0,"y":0.0},{"x":4.125,"y":0.0},{"x":4.25,"y":0.0},{"x":4.375,"y":0.0},{"x":4.5,"y":0.0},{"x":4.625,"y":0.0},{"x":4.75,"y":0.0},{"x":4.875,"y":0.0},{"x":5.0,"y":0.0},{"x":5.125,"y":0.0},{"x":5.25,"y":0}]},{"name":"1a2252dd-29d9-4c15-b037-79afa7677080","values":[{"x":-5.0,"y":0},{"x":-4.75,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.75,"y":0.0},{"x":-0.5,"y":0.0},{"x":-0.25,"y":0.0},{"x":0.0,"y":0.0},{"x":0.25,"y":0.0},{"x":0.5,"y":0.0012},{"x":0.75,"y":0.0164},{"x":1.0,"y":0.066},{"x":1.25,"y":0.1508},{"x":1.5,"y":0.2352},{"x":1.75,"y":0.2864},{"x":2.0,"y":0.3392},{"x":2.25,"y":0.3264},{"x":2.5,"y":0.3172},{"x":2.75,"y":0.306},{"x":3.0,"y":0.2872},{"x":3.25,"y":0.2304},{"x":3.5,"y":0.218},{"x":3.75,"y":0.1884},{"x":4.0,"y":0.1564},{"x":4.25,"y":0.1328},{"x":4.5,"y":0.1032},{"x":4.75,"y":0.096},{"x":5.0,"y":0.0988},{"x":5.25,"y":0.0656},{"x":5.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"f177f054-0598-4b8b-a288-54cf303c1ad6"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"1a2252dd-29d9-4c15-b037-79afa7677080"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-5 5]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"f177f054-0598-4b8b-a288-54cf303c1ad6\", :values ({:x -5.0, :y 0} {:x -4.875, :y 0.0} {:x -4.75, :y 0.0} {:x -4.625, :y 0.0} {:x -4.5, :y 0.0} {:x -4.375, :y 0.0} {:x -4.25, :y 0.0} {:x -4.125, :y 0.0} {:x -4.0, :y 0.0} {:x -3.875, :y 0.0} {:x -3.75, :y 0.0} {:x -3.625, :y 0.0} {:x -3.5, :y 0.0} {:x -3.375, :y 0.0} {:x -3.25, :y 0.0} {:x -3.125, :y 0.0} {:x -3.0, :y 0.0} {:x -2.875, :y 0.0} {:x -2.75, :y 0.0} {:x -2.625, :y 0.0} {:x -2.5, :y 0.0} {:x -2.375, :y 0.0} {:x -2.25, :y 0.0} {:x -2.125, :y 0.0} {:x -2.0, :y 0.0} {:x -1.875, :y 0.0} {:x -1.75, :y 0.0} {:x -1.625, :y 0.0} {:x -1.5, :y 0.0} {:x -1.375, :y 0.0} {:x -1.25, :y 0.0} {:x -1.125, :y 0.0} {:x -1.0, :y 0.0} {:x -0.875, :y 8.0E-4} {:x -0.75, :y 0.0016} {:x -0.625, :y 0.004} {:x -0.5, :y 0.0064} {:x -0.375, :y 0.0152} {:x -0.25, :y 0.0272} {:x -0.125, :y 0.0536} {:x 0.0, :y 0.088} {:x 0.125, :y 0.1408} {:x 0.25, :y 0.2344} {:x 0.375, :y 0.3424} {:x 0.5, :y 0.4152} {:x 0.625, :y 0.492} {:x 0.75, :y 0.6712} {:x 0.875, :y 0.7784} {:x 1.0, :y 0.7792} {:x 1.125, :y 0.8536} {:x 1.25, :y 0.716} {:x 1.375, :y 0.616} {:x 1.5, :y 0.5344} {:x 1.625, :y 0.4224} {:x 1.75, :y 0.3112} {:x 1.875, :y 0.1928} {:x 2.0, :y 0.1248} {:x 2.125, :y 0.0808} {:x 2.25, :y 0.044} {:x 2.375, :y 0.0344} {:x 2.5, :y 0.0128} {:x 2.625, :y 0.0032} {:x 2.75, :y 0.0032} {:x 2.875, :y 0.0} {:x 3.0, :y 0.0} {:x 3.125, :y 0.0} {:x 3.25, :y 0.0} {:x 3.375, :y 0.0} {:x 3.5, :y 0.0} {:x 3.625, :y 0.0} {:x 3.75, :y 0.0} {:x 3.875, :y 0.0} {:x 4.0, :y 0.0} {:x 4.125, :y 0.0} {:x 4.25, :y 0.0} {:x 4.375, :y 0.0} {:x 4.5, :y 0.0} {:x 4.625, :y 0.0} {:x 4.75, :y 0.0} {:x 4.875, :y 0.0} {:x 5.0, :y 0.0} {:x 5.125, :y 0.0} {:x 5.25, :y 0})} {:name \"1a2252dd-29d9-4c15-b037-79afa7677080\", :values ({:x -5.0, :y 0} {:x -4.75, :y 0.0} {:x -4.5, :y 0.0} {:x -4.25, :y 0.0} {:x -4.0, :y 0.0} {:x -3.75, :y 0.0} {:x -3.5, :y 0.0} {:x -3.25, :y 0.0} {:x -3.0, :y 0.0} {:x -2.75, :y 0.0} {:x -2.5, :y 0.0} {:x -2.25, :y 0.0} {:x -2.0, :y 0.0} {:x -1.75, :y 0.0} {:x -1.5, :y 0.0} {:x -1.25, :y 0.0} {:x -1.0, :y 0.0} {:x -0.75, :y 0.0} {:x -0.5, :y 0.0} {:x -0.25, :y 0.0} {:x 0.0, :y 0.0} {:x 0.25, :y 0.0} {:x 0.5, :y 0.0012} {:x 0.75, :y 0.0164} {:x 1.0, :y 0.066} {:x 1.25, :y 0.1508} {:x 1.5, :y 0.2352} {:x 1.75, :y 0.2864} {:x 2.0, :y 0.3392} {:x 2.25, :y 0.3264} {:x 2.5, :y 0.3172} {:x 2.75, :y 0.306} {:x 3.0, :y 0.2872} {:x 3.25, :y 0.2304} {:x 3.5, :y 0.218} {:x 3.75, :y 0.1884} {:x 4.0, :y 0.1564} {:x 4.25, :y 0.1328} {:x 4.5, :y 0.1032} {:x 4.75, :y 0.096} {:x 5.0, :y 0.0988} {:x 5.25, :y 0.0656} {:x 5.5, :y 0})}), :marks ({:type \"line\", :from {:data \"f177f054-0598-4b8b-a288-54cf303c1ad6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"1a2252dd-29d9-4c15-b037-79afa7677080\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-percent-distrib</span>","value":"#'financial-forecasting/div-percent-distrib"}
;; <=

;; @@
(with-primitive-procedures [log-normal]
  (defquery at-given-dt-Ft [Ft dt]
    ; This is a model (simulator) for amount of dividents
    (sample 
      (if dt
        (let [div-percent (div-percent-distrib Ft)]
          (log-normal
            (* div-percent (:stock-price Ft)) 1))
        (dirac 0)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/at-given-dt-Ft</span>","value":"#'financial-forecasting/at-given-dt-Ft"}
;; <=

;; @@
;(def Ft-data-0 (get Ft-data :'30-06-2013'))
(def Ft-data-0 (get Ft-data :'31-03-2014'))

(print Ft-data-0)

(->> (doquery :smc
  at-given-dt-Ft [Ft-data-0 true] ; TODO: fake line
  ;at-given-dt-Ft [Ft-data-0]
  :number-of-particles 100)
     (take 5000)
     stat/collect-results
     (stat/empirical-mean))

;; @@
;; ->
;;; {:stock-price 70.97, :volume 50194333, :div 0.6, :net-income 0.65, :spike 0}
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1.1374296117166369E30</span>","value":"1.1374296117166369E30"}
;; <=

;; @@

;; @@

;; @@

;; @@
