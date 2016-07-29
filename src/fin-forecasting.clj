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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-5,5]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"15f65a9c-54bb-49cb-8e7f-8cd456bbb691","values":[{"x":-5.0,"y":0},{"x":-4.875,"y":0.0},{"x":-4.75,"y":0.0},{"x":-4.625,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.375,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.125,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.875,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.625,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.375,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.125,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.875,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.625,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.375,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.125,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.875,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.625,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.375,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.125,"y":0.0},{"x":-1.0,"y":8.0E-4},{"x":-0.875,"y":0.0},{"x":-0.75,"y":0.0032},{"x":-0.625,"y":8.0E-4},{"x":-0.5,"y":0.0032},{"x":-0.375,"y":0.0136},{"x":-0.25,"y":0.028},{"x":-0.125,"y":0.0424},{"x":0.0,"y":0.0888},{"x":0.125,"y":0.1392},{"x":0.25,"y":0.232},{"x":0.375,"y":0.3296},{"x":0.5,"y":0.4352},{"x":0.625,"y":0.5776},{"x":0.75,"y":0.636},{"x":0.875,"y":0.7328},{"x":1.0,"y":0.7608},{"x":1.125,"y":0.7816},{"x":1.25,"y":0.7424},{"x":1.375,"y":0.6544},{"x":1.5,"y":0.5424},{"x":1.625,"y":0.4016},{"x":1.75,"y":0.3072},{"x":1.875,"y":0.228},{"x":2.0,"y":0.136},{"x":2.125,"y":0.0824},{"x":2.25,"y":0.0448},{"x":2.375,"y":0.028},{"x":2.5,"y":0.0128},{"x":2.625,"y":0.0096},{"x":2.75,"y":0.0024},{"x":2.875,"y":8.0E-4},{"x":3.0,"y":8.0E-4},{"x":3.125,"y":0.0},{"x":3.25,"y":8.0E-4},{"x":3.375,"y":0.0},{"x":3.5,"y":0.0},{"x":3.625,"y":0.0},{"x":3.75,"y":0.0},{"x":3.875,"y":0.0},{"x":4.0,"y":0.0},{"x":4.125,"y":0.0},{"x":4.25,"y":0.0},{"x":4.375,"y":0.0},{"x":4.5,"y":0.0},{"x":4.625,"y":0.0},{"x":4.75,"y":0.0},{"x":4.875,"y":0.0},{"x":5.0,"y":0.0},{"x":5.125,"y":0.0},{"x":5.25,"y":0}]},{"name":"8c964a45-4ec1-4a30-b1fd-6e8ffc5cd649","values":[{"x":-5.0,"y":0},{"x":-4.75,"y":0.0},{"x":-4.5,"y":0.0},{"x":-4.25,"y":0.0},{"x":-4.0,"y":0.0},{"x":-3.75,"y":0.0},{"x":-3.5,"y":0.0},{"x":-3.25,"y":0.0},{"x":-3.0,"y":0.0},{"x":-2.75,"y":0.0},{"x":-2.5,"y":0.0},{"x":-2.25,"y":0.0},{"x":-2.0,"y":0.0},{"x":-1.75,"y":0.0},{"x":-1.5,"y":0.0},{"x":-1.25,"y":0.0},{"x":-1.0,"y":4.0E-4},{"x":-0.75,"y":0.0016},{"x":-0.5,"y":0.002},{"x":-0.25,"y":0.0208},{"x":0.0,"y":0.0656},{"x":0.25,"y":0.1856},{"x":0.5,"y":0.3824},{"x":0.75,"y":0.6068},{"x":1.0,"y":0.7468},{"x":1.25,"y":0.762},{"x":1.5,"y":0.5984},{"x":1.75,"y":0.3544},{"x":2.0,"y":0.182},{"x":2.25,"y":0.0636},{"x":2.5,"y":0.0204},{"x":2.75,"y":0.006},{"x":3.0,"y":8.0E-4},{"x":3.25,"y":4.0E-4},{"x":3.5,"y":0.0},{"x":3.75,"y":0.0},{"x":4.0,"y":0.0},{"x":4.25,"y":0.0},{"x":4.5,"y":0.0},{"x":4.75,"y":0.0},{"x":5.0,"y":0.0},{"x":5.25,"y":0.0},{"x":5.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"15f65a9c-54bb-49cb-8e7f-8cd456bbb691"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"8c964a45-4ec1-4a30-b1fd-6e8ffc5cd649"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-5 5]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"15f65a9c-54bb-49cb-8e7f-8cd456bbb691\", :values ({:x -5.0, :y 0} {:x -4.875, :y 0.0} {:x -4.75, :y 0.0} {:x -4.625, :y 0.0} {:x -4.5, :y 0.0} {:x -4.375, :y 0.0} {:x -4.25, :y 0.0} {:x -4.125, :y 0.0} {:x -4.0, :y 0.0} {:x -3.875, :y 0.0} {:x -3.75, :y 0.0} {:x -3.625, :y 0.0} {:x -3.5, :y 0.0} {:x -3.375, :y 0.0} {:x -3.25, :y 0.0} {:x -3.125, :y 0.0} {:x -3.0, :y 0.0} {:x -2.875, :y 0.0} {:x -2.75, :y 0.0} {:x -2.625, :y 0.0} {:x -2.5, :y 0.0} {:x -2.375, :y 0.0} {:x -2.25, :y 0.0} {:x -2.125, :y 0.0} {:x -2.0, :y 0.0} {:x -1.875, :y 0.0} {:x -1.75, :y 0.0} {:x -1.625, :y 0.0} {:x -1.5, :y 0.0} {:x -1.375, :y 0.0} {:x -1.25, :y 0.0} {:x -1.125, :y 0.0} {:x -1.0, :y 8.0E-4} {:x -0.875, :y 0.0} {:x -0.75, :y 0.0032} {:x -0.625, :y 8.0E-4} {:x -0.5, :y 0.0032} {:x -0.375, :y 0.0136} {:x -0.25, :y 0.028} {:x -0.125, :y 0.0424} {:x 0.0, :y 0.0888} {:x 0.125, :y 0.1392} {:x 0.25, :y 0.232} {:x 0.375, :y 0.3296} {:x 0.5, :y 0.4352} {:x 0.625, :y 0.5776} {:x 0.75, :y 0.636} {:x 0.875, :y 0.7328} {:x 1.0, :y 0.7608} {:x 1.125, :y 0.7816} {:x 1.25, :y 0.7424} {:x 1.375, :y 0.6544} {:x 1.5, :y 0.5424} {:x 1.625, :y 0.4016} {:x 1.75, :y 0.3072} {:x 1.875, :y 0.228} {:x 2.0, :y 0.136} {:x 2.125, :y 0.0824} {:x 2.25, :y 0.0448} {:x 2.375, :y 0.028} {:x 2.5, :y 0.0128} {:x 2.625, :y 0.0096} {:x 2.75, :y 0.0024} {:x 2.875, :y 8.0E-4} {:x 3.0, :y 8.0E-4} {:x 3.125, :y 0.0} {:x 3.25, :y 8.0E-4} {:x 3.375, :y 0.0} {:x 3.5, :y 0.0} {:x 3.625, :y 0.0} {:x 3.75, :y 0.0} {:x 3.875, :y 0.0} {:x 4.0, :y 0.0} {:x 4.125, :y 0.0} {:x 4.25, :y 0.0} {:x 4.375, :y 0.0} {:x 4.5, :y 0.0} {:x 4.625, :y 0.0} {:x 4.75, :y 0.0} {:x 4.875, :y 0.0} {:x 5.0, :y 0.0} {:x 5.125, :y 0.0} {:x 5.25, :y 0})} {:name \"8c964a45-4ec1-4a30-b1fd-6e8ffc5cd649\", :values ({:x -5.0, :y 0} {:x -4.75, :y 0.0} {:x -4.5, :y 0.0} {:x -4.25, :y 0.0} {:x -4.0, :y 0.0} {:x -3.75, :y 0.0} {:x -3.5, :y 0.0} {:x -3.25, :y 0.0} {:x -3.0, :y 0.0} {:x -2.75, :y 0.0} {:x -2.5, :y 0.0} {:x -2.25, :y 0.0} {:x -2.0, :y 0.0} {:x -1.75, :y 0.0} {:x -1.5, :y 0.0} {:x -1.25, :y 0.0} {:x -1.0, :y 4.0E-4} {:x -0.75, :y 0.0016} {:x -0.5, :y 0.002} {:x -0.25, :y 0.0208} {:x 0.0, :y 0.0656} {:x 0.25, :y 0.1856} {:x 0.5, :y 0.3824} {:x 0.75, :y 0.6068} {:x 1.0, :y 0.7468} {:x 1.25, :y 0.762} {:x 1.5, :y 0.5984} {:x 1.75, :y 0.3544} {:x 2.0, :y 0.182} {:x 2.25, :y 0.0636} {:x 2.5, :y 0.0204} {:x 2.75, :y 0.006} {:x 3.0, :y 8.0E-4} {:x 3.25, :y 4.0E-4} {:x 3.5, :y 0.0} {:x 3.75, :y 0.0} {:x 4.0, :y 0.0} {:x 4.25, :y 0.0} {:x 4.5, :y 0.0} {:x 4.75, :y 0.0} {:x 5.0, :y 0.0} {:x 5.25, :y 0.0} {:x 5.5, :y 0})}), :marks ({:type \"line\", :from {:data \"15f65a9c-54bb-49cb-8e7f-8cd456bbb691\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"8c964a45-4ec1-4a30-b1fd-6e8ffc5cd649\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@
(defdist dirac
  "Dirac distribution"
  [x]  ; distribution parameters
  []   ; auxiliary bindings
  (sample* [this] x)
  (observe* [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))
;; @@

;; @@
(defm div-percent-distrib [Ft]
  (> (:volume Ft) 0.5))
    
;; @@

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
