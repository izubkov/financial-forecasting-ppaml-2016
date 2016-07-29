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
  (let [ ; we do not know our distrib. yes - we'll learn it
         do-inc-distrib     (sample (uniform-continuous 0. 1.))
         do-not-inc-distrib (sample (uniform-continuous 0. 1.))]
    (loop [fi (first Ft) ; first quarter
           fts (rest Ft)
           model-param-inc   do-inc-distrib ; initial inc distrib.
           model-param-noinc do-not-inc-distrib] ; initial no-inc distrib.
      (if fts
        ((let [addition-distrib
               (amount-to-increase-div-distrib
                 (div-increase-boolean fi do-inc-distrib do-not-inc-distrib)
                 0.05 0.1)]
           (observe (addition-distrib) do-inc-distrib)
           (observe (addition-distrib) do-not-inc-distrib))
         (recur (first fts) (rest fts) do-inc-distrib do-not-inc-distrib))
        ({} () do-inc-distrib do-not-inc-distrib)))
    {:do-inc-distrib     do-inc-distrib
     :do-not-inc-distrib do-not-inc-distrib}))
;;(loop [futurequarters []]
;;  .... with sample)}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/at-given-dt-Ft</span>","value":"#'financial-forecasting/at-given-dt-Ft"}
;; <=

;; @@
(->> (doquery :smc
  at-given-dt-Ft [Ft-data] ; feed previous frame (Ft) here
  :number-of-particles 100)
     (take 5000)
     stat/collect-results
     (stat/empirical-mean))
;; @@

;; @@

;; @@

;; @@

;; @@
