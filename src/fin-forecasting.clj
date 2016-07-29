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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn log-normal [x mu sigma]
  (if (> sigma 0)
    (observe (normal mu sigma) (Math/log x))
    (Math/log 0.0)))

(def log-samples
  (repeatedly 10000 #(sample (normal 1 0.5))))

(plot/compose
  (plot/histogram log-samples :normalise
                              :probability-density
                              :bins 80
                              :plot-range [[-5 5] [0 0.8]])
  (plot/histogram log-samples :normalise
                              :probability-density
                              :bins 40
                              :plot-range [[-5 5] [0 0.8]]))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-5,5]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"ea36b5c1-10a2-4678-bee4-50c693c3f413","values":[{"x":-5.0,"y":0},{"x":-4.875,"y":0.0},{"x":-4.75,"y":0.0},{"x":-4.625,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.375,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.125,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.875,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.625,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.375,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.125,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.875,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.625,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.375,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.125,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.875,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.625,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.375,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.125,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.875,"y":0.0},{"x":-0.75,"y":0.0},{"x":-0.625,"y":0.0048},{"x":-0.5,"y":0.0064},{"x":-0.375,"y":0.0144},{"x":-0.25,"y":0.0296},{"x":-0.125,"y":0.0472},{"x":0.0,"y":0.084},{"x":0.125,"y":0.1304},{"x":0.25,"y":0.2016},{"x":0.375,"y":0.3176},{"x":0.5,"y":0.4176},{"x":0.625,"y":0.5168},{"x":0.75,"y":0.648},{"x":0.875,"y":0.6936},{"x":1.0,"y":0.8208},{"x":1.125,"y":0.8424},{"x":1.25,"y":0.716},{"x":1.375,"y":0.668},{"x":1.5,"y":0.5432},{"x":1.625,"y":0.4176},{"x":1.75,"y":0.3016},{"x":1.875,"y":0.2368},{"x":2.0,"y":0.14},{"x":2.125,"y":0.0856},{"x":2.25,"y":0.0568},{"x":2.375,"y":0.028},{"x":2.5,"y":0.0176},{"x":2.625,"y":0.0072},{"x":2.75,"y":0.004},{"x":2.875,"y":0.0016},{"x":3.0,"y":0.0},{"x":3.125,"y":8.0E-4},{"x":3.25,"y":0.0},{"x":3.375,"y":0.0},{"x":3.5,"y":0.0},{"x":3.625,"y":0.0},{"x":3.75,"y":0.0},{"x":3.875,"y":0.0},{"x":4.0,"y":0.0},{"x":4.125,"y":0.0},{"x":4.25,"y":0.0},{"x":4.375,"y":0.0},{"x":4.5,"y":0.0},{"x":4.625,"y":0.0},{"x":4.75,"y":0.0},{"x":4.875,"y":0.0},{"x":5.0,"y":0.0},{"x":5.125,"y":0.0},{"x":5.25,"y":0}]},{"name":"eaef10a2-0cb8-4dcc-a894-85d9f6bbd62f","values":[{"x":-5.0,"y":0},{"x":-4.75,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.0,"y":0.0},{"x":-0.75,"y":0.0},{"x":-0.5,"y":0.0056},{"x":-0.25,"y":0.022},{"x":0.0,"y":0.0656},{"x":0.25,"y":0.166},{"x":0.5,"y":0.3676},{"x":0.75,"y":0.5824},{"x":1.0,"y":0.7572},{"x":1.25,"y":0.7792},{"x":1.5,"y":0.6056},{"x":1.75,"y":0.3596},{"x":2.0,"y":0.1884},{"x":2.25,"y":0.0712},{"x":2.5,"y":0.0228},{"x":2.75,"y":0.0056},{"x":3.0,"y":8.0E-4},{"x":3.25,"y":4.0E-4},{"x":3.5,"y":0.0},{"x":3.75,"y":0.0},{"x":4.0,"y":0.0},{"x":4.25,"y":0.0},{"x":4.5,"y":0.0},{"x":4.75,"y":0.0},{"x":5.0,"y":0.0},{"x":5.25,"y":0.0},{"x":5.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"ea36b5c1-10a2-4678-bee4-50c693c3f413"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"eaef10a2-0cb8-4dcc-a894-85d9f6bbd62f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-5 5]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"ea36b5c1-10a2-4678-bee4-50c693c3f413\", :values ({:x -5.0, :y 0} {:x -4.875, :y 0.0} {:x -4.75, :y 0.0} {:x -4.625, :y 0.0} {:x -4.5, :y 0.0} {:x -4.375, :y 0.0} {:x -4.25, :y 0.0} {:x -4.125, :y 0.0} {:x -4.0, :y 0.0} {:x -3.875, :y 0.0} {:x -3.75, :y 0.0} {:x -3.625, :y 0.0} {:x -3.5, :y 0.0} {:x -3.375, :y 0.0} {:x -3.25, :y 0.0} {:x -3.125, :y 0.0} {:x -3.0, :y 0.0} {:x -2.875, :y 0.0} {:x -2.75, :y 0.0} {:x -2.625, :y 0.0} {:x -2.5, :y 0.0} {:x -2.375, :y 0.0} {:x -2.25, :y 0.0} {:x -2.125, :y 0.0} {:x -2.0, :y 0.0} {:x -1.875, :y 0.0} {:x -1.75, :y 0.0} {:x -1.625, :y 0.0} {:x -1.5, :y 0.0} {:x -1.375, :y 0.0} {:x -1.25, :y 0.0} {:x -1.125, :y 0.0} {:x -1.0, :y 0.0} {:x -0.875, :y 0.0} {:x -0.75, :y 0.0} {:x -0.625, :y 0.0048} {:x -0.5, :y 0.0064} {:x -0.375, :y 0.0144} {:x -0.25, :y 0.0296} {:x -0.125, :y 0.0472} {:x 0.0, :y 0.084} {:x 0.125, :y 0.1304} {:x 0.25, :y 0.2016} {:x 0.375, :y 0.3176} {:x 0.5, :y 0.4176} {:x 0.625, :y 0.5168} {:x 0.75, :y 0.648} {:x 0.875, :y 0.6936} {:x 1.0, :y 0.8208} {:x 1.125, :y 0.8424} {:x 1.25, :y 0.716} {:x 1.375, :y 0.668} {:x 1.5, :y 0.5432} {:x 1.625, :y 0.4176} {:x 1.75, :y 0.3016} {:x 1.875, :y 0.2368} {:x 2.0, :y 0.14} {:x 2.125, :y 0.0856} {:x 2.25, :y 0.0568} {:x 2.375, :y 0.028} {:x 2.5, :y 0.0176} {:x 2.625, :y 0.0072} {:x 2.75, :y 0.004} {:x 2.875, :y 0.0016} {:x 3.0, :y 0.0} {:x 3.125, :y 8.0E-4} {:x 3.25, :y 0.0} {:x 3.375, :y 0.0} {:x 3.5, :y 0.0} {:x 3.625, :y 0.0} {:x 3.75, :y 0.0} {:x 3.875, :y 0.0} {:x 4.0, :y 0.0} {:x 4.125, :y 0.0} {:x 4.25, :y 0.0} {:x 4.375, :y 0.0} {:x 4.5, :y 0.0} {:x 4.625, :y 0.0} {:x 4.75, :y 0.0} {:x 4.875, :y 0.0} {:x 5.0, :y 0.0} {:x 5.125, :y 0.0} {:x 5.25, :y 0})} {:name \"eaef10a2-0cb8-4dcc-a894-85d9f6bbd62f\", :values ({:x -5.0, :y 0} {:x -4.75, :y 0.0} {:x -4.5, :y 0.0} {:x -4.25, :y 0.0} {:x -4.0, :y 0.0} {:x -3.75, :y 0.0} {:x -3.5, :y 0.0} {:x -3.25, :y 0.0} {:x -3.0, :y 0.0} {:x -2.75, :y 0.0} {:x -2.5, :y 0.0} {:x -2.25, :y 0.0} {:x -2.0, :y 0.0} {:x -1.75, :y 0.0} {:x -1.5, :y 0.0} {:x -1.25, :y 0.0} {:x -1.0, :y 0.0} {:x -0.75, :y 0.0} {:x -0.5, :y 0.0056} {:x -0.25, :y 0.022} {:x 0.0, :y 0.0656} {:x 0.25, :y 0.166} {:x 0.5, :y 0.3676} {:x 0.75, :y 0.5824} {:x 1.0, :y 0.7572} {:x 1.25, :y 0.7792} {:x 1.5, :y 0.6056} {:x 1.75, :y 0.3596} {:x 2.0, :y 0.1884} {:x 2.25, :y 0.0712} {:x 2.5, :y 0.0228} {:x 2.75, :y 0.0056} {:x 3.0, :y 8.0E-4} {:x 3.25, :y 4.0E-4} {:x 3.5, :y 0.0} {:x 3.75, :y 0.0} {:x 4.0, :y 0.0} {:x 4.25, :y 0.0} {:x 4.5, :y 0.0} {:x 4.75, :y 0.0} {:x 5.0, :y 0.0} {:x 5.25, :y 0.0} {:x 5.5, :y 0})}), :marks ({:type \"line\", :from {:data \"ea36b5c1-10a2-4678-bee4-50c693c3f413\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"eaef10a2-0cb8-4dcc-a894-85d9f6bbd62f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@
(defdist dirac
  "Dirac distribution"
  [x]  ; distribution parameters
  []   ; auxiliary bindings
  (sample [this] x)
  (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@3635b8&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@3635b8>"}
;; <=

;; @@
(defm div-percent-distrib [Ft]
  (> (:volume Ft) 0.5))
    
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-percent-distrib</span>","value":"#'financial-forecasting/div-percent-distrib"}
;; <=

;; @@
(defm at-given-dt-Ft [Ft dt div-percent-distrib]
  (if dt
    (let [div-percent (sample div-percent-distrib)]
      (log-normal
        (* div-percent (:earnings Ft) 1))
      (dirac 0))
    )
  )
;; @@

;; @@
(doquery :smc
  at-given-dt-Ft [xval]
  :number-of-particles 100)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-percent-distrib</span>","value":"#'financial-forecasting/div-percent-distrib"}
;; <=
