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
(defm div-increase-boolean [Ft do-inc not-do-inc] ; Dt
  ; takes F(t-1) spike (for example, prev. september)
  ; returns boolean
  (if (= (:spike Ft) 1)            ; spike in september
      (sample (flip do-inc))       ; high prob. of increase
      (sample (flip not-do-inc)))) ; low prob.
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-increase-boolean</span>","value":"#'financial-forecasting/div-increase-boolean"}
;; <=

;; @@
(defm amount-to-increase-div-distrib [dt a b]
  (if dt ; decision to increase or not to increase dividents
    (uniform-continuous a b)
    (uniform-continuous 0. 0.0000000001)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/amount-to-increase-div-distrib</span>","value":"#'financial-forecasting/amount-to-increase-div-distrib"}
;; <=

;; @@
(defquery at-given-dt-Ft [Ft]
  ; This is a model (simulator) for amount of dividents for next quarter
  (let [pifspike (sample (uniform-continuous 0. 1.)) ; we do not know a distrib.
        ; we learn it
        pifnospike (sample (uniform-continuous 0. 1.))]
    (loop [fi (first Ft) ; first quarter
           localrvswecareabout []]
      (let [addition-distrib
            (amount-to-increase-div-distrib (div-increase-boolean 
                                              Ft pifspike 
                                              pifnospike))]
        (observe (addition-distrib) pifnospike pifspike)
        (recur (next ...) [])))
    {:pifspike pifspike
     :pifnospike pifnospike
     (loop [futurequarters []]
       .... with sample)}))
;; @@

;; @@
Ft-data
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;31-03-2014&#x27;</span>","value":":'31-03-2014'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>70.97</span>","value":"70.97"}],"value":"[:stock-price 70.97]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>50194333</span>","value":"50194333"}],"value":"[:volume 50194333]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.6</span>","value":"0.6"}],"value":"[:div 0.6]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.65</span>","value":"0.65"}],"value":"[:net-income 0.65]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:spike 0]"}],"value":"{:stock-price 70.97, :volume 50194333, :div 0.6, :net-income 0.65, :spike 0}"}],"value":"[:'31-03-2014' {:stock-price 70.97, :volume 50194333, :div 0.6, :net-income 0.65, :spike 0}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;30-06-2014&#x27;</span>","value":":'30-06-2014'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>71.0</span>","value":"71.0"}],"value":"[:stock-price 71.0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>49018766</span>","value":"49018766"}],"value":"[:volume 49018766]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.6</span>","value":"0.6"}],"value":"[:div 0.6]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.27</span>","value":"0.27"}],"value":"[:net-income 0.27]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:spike 0]"}],"value":"{:stock-price 71.0, :volume 49018766, :div 0.6, :net-income 0.27, :spike 0}"}],"value":"[:'30-06-2014' {:stock-price 71.0, :volume 49018766, :div 0.6, :net-income 0.27, :spike 0}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;30-03-2016&#x27;</span>","value":":'30-03-2016'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>72.52</span>","value":"72.52"}],"value":"[:stock-price 72.52]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>69868350</span>","value":"69868350"}],"value":"[:volume 69868350]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.7</span>","value":"0.7"}],"value":"[:div 0.7]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.88</span>","value":"0.88"}],"value":"[:net-income 0.88]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:spike 0]"}],"value":"{:stock-price 72.52, :volume 69868350, :div 0.7, :net-income 0.88, :spike 0}"}],"value":"[:'30-03-2016' {:stock-price 72.52, :volume 69868350, :div 0.7, :net-income 0.88, :spike 0}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;30-06-2013&#x27;</span>","value":":'30-06-2013'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>58.35</span>","value":"58.35"}],"value":"[:stock-price 58.35]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>60551066</span>","value":"60551066"}],"value":"[:volume 60551066]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.5625</span>","value":"0.5625"}],"value":"[:div 0.5625]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.35</span>","value":"0.35"}],"value":"[:net-income 0.35]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:spike 0]"}],"value":"{:stock-price 58.35, :volume 60551066, :div 0.5625, :net-income 0.35, :spike 0}"}],"value":"[:'30-06-2013' {:stock-price 58.35, :volume 60551066, :div 0.5625, :net-income 0.35, :spike 0}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;30-09-2014&#x27;</span>","value":":'30-09-2014'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>68.98</span>","value":"68.98"}],"value":"[:stock-price 68.98]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>45221900</span>","value":"45221900"}],"value":"[:volume 45221900]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.6</span>","value":"0.6"}],"value":"[:div 0.6]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.9</span>","value":"0.9"}],"value":"[:net-income 0.9]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:spike 1]"}],"value":"{:stock-price 68.98, :volume 45221900, :div 0.6, :net-income 0.9, :spike 1}"}],"value":"[:'30-09-2014' {:stock-price 68.98, :volume 45221900, :div 0.6, :net-income 0.9, :spike 1}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;30-09-2015&#x27;</span>","value":":'30-09-2015'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>70.61</span>","value":"70.61"}],"value":"[:stock-price 70.61]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>56544466</span>","value":"56544466"}],"value":"[:volume 56544466]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.6475</span>","value":"0.6475"}],"value":"[:div 0.6475]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>1.0</span>","value":"1.0"}],"value":"[:net-income 1.0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:spike 1]"}],"value":"{:stock-price 70.61, :volume 56544466, :div 0.6475, :net-income 1.0, :spike 1}"}],"value":"[:'30-09-2015' {:stock-price 70.61, :volume 56544466, :div 0.6475, :net-income 1.0, :spike 1}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;30-09-2013&#x27;</span>","value":":'30-09-2013'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>60.046</span>","value":"60.046"}],"value":"[:stock-price 60.046]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>57507000</span>","value":"57507000"}],"value":"[:volume 57507000]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.5625</span>","value":"0.5625"}],"value":"[:div 0.5625]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.98</span>","value":"0.98"}],"value":"[:net-income 0.98]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:spike 1]"}],"value":"{:stock-price 60.046, :volume 57507000, :div 0.5625, :net-income 0.98, :spike 1}"}],"value":"[:'30-09-2013' {:stock-price 60.046, :volume 57507000, :div 0.5625, :net-income 0.98, :spike 1}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;31-03-2015&#x27;</span>","value":":'31-03-2015'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>71.48</span>","value":"71.48"}],"value":"[:stock-price 71.48]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>63794600</span>","value":"63794600"}],"value":"[:volume 63794600]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.65</span>","value":"0.65"}],"value":"[:div 0.65]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.91</span>","value":"0.91"}],"value":"[:net-income 0.91]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:spike 0]"}],"value":"{:stock-price 71.48, :volume 63794600, :div 0.65, :net-income 0.91, :spike 0}"}],"value":"[:'31-03-2015' {:stock-price 71.48, :volume 63794600, :div 0.65, :net-income 0.91, :spike 0}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:&#x27;30-06-2015&#x27;</span>","value":":'30-06-2015'"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stock-price</span>","value":":stock-price"},{"type":"html","content":"<span class='clj-double'>69.69</span>","value":"69.69"}],"value":"[:stock-price 69.69]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:volume</span>","value":":volume"},{"type":"html","content":"<span class='clj-long'>52825033</span>","value":"52825033"}],"value":"[:volume 52825033]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:div</span>","value":":div"},{"type":"html","content":"<span class='clj-double'>0.6475</span>","value":"0.6475"}],"value":"[:div 0.6475]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:net-income</span>","value":":net-income"},{"type":"html","content":"<span class='clj-double'>0.7</span>","value":"0.7"}],"value":"[:net-income 0.7]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:spike</span>","value":":spike"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[:spike 0]"}],"value":"{:stock-price 69.69, :volume 52825033, :div 0.6475, :net-income 0.7, :spike 0}"}],"value":"[:'30-06-2015' {:stock-price 69.69, :volume 52825033, :div 0.6475, :net-income 0.7, :spike 0}]"}],"value":"{:'31-03-2014' {:stock-price 70.97, :volume 50194333, :div 0.6, :net-income 0.65, :spike 0}, :'30-06-2014' {:stock-price 71.0, :volume 49018766, :div 0.6, :net-income 0.27, :spike 0}, :'30-03-2016' {:stock-price 72.52, :volume 69868350, :div 0.7, :net-income 0.88, :spike 0}, :'30-06-2013' {:stock-price 58.35, :volume 60551066, :div 0.5625, :net-income 0.35, :spike 0}, :'30-09-2014' {:stock-price 68.98, :volume 45221900, :div 0.6, :net-income 0.9, :spike 1}, :'30-09-2015' {:stock-price 70.61, :volume 56544466, :div 0.6475, :net-income 1.0, :spike 1}, :'30-09-2013' {:stock-price 60.046, :volume 57507000, :div 0.5625, :net-income 0.98, :spike 1}, :'31-03-2015' {:stock-price 71.48, :volume 63794600, :div 0.65, :net-income 0.91, :spike 0}, :'30-06-2015' {:stock-price 69.69, :volume 52825033, :div 0.6475, :net-income 0.7, :spike 0}}"}
;; <=

;; @@
;(def Ft-data-0 (get Ft-data :'30-06-2013'))
;(def Ft-data-0 (get Ft-data :'30-09-2013'))
;(def Ft-data-0 (get Ft-data :'30-06-2015'))
;(def Ft-data-0 (get Ft-data :'30-09-2015'))

(println Ft-data-0)

(->> (doquery :smc
  at-given-dt-Ft [Ft-data] ; feed previous frame (Ft) here
  :number-of-particles 100)
     (take 5000)
     stat/collect-results
     (stat/empirical-mean))

;; @@
;; ->
;;; {:stock-price 69.69, :volume 52825033, :div 0.6475, :net-income 0.7, :spike 0}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.006912636317679237</span>","value":"0.006912636317679237"}
;; <=

;; @@

;; @@

;; @@

;; @@
