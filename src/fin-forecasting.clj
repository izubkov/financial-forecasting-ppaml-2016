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
(defm log-normal
  (normal mean std))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/log-normal</span>","value":"#'financial-forecasting/log-normal"}
;; <=

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
