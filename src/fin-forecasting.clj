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
(defm div-increase-sample [Ft do-inc not-do-inc] ; Dt
  ; takes F(t-1) spike (for example, prev. september)
  ; returns samples
  (if (= (:spike Ft) 1)            ; spike in september
      do-inc       ; high prob. of increase
      not-do-inc)) ; low prob.
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/div-increase-sample</span>","value":"#'financial-forecasting/div-increase-sample"}
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
           m-inc    do-inc-distrib ; initial inc distrib.
           m-no-inc do-not-inc-distrib] ; initial no-inc distrib.
      (if (seq fts)
        (let [addition-distrib                                                 
              (amount-to-increase-div-distrib
                (div-increase-sample fi m-inc m-no-inc)
                0.05 0.1)]
          (observe addition-distrib m-inc)                           
          (observe addition-distrib m-no-inc)                       
          (recur (first fts) 
                 (rest fts) 
                 m-inc 
                 m-no-inc))
        {:do-inc-distrib     m-inc
         :do-not-inc-distrib m-no-inc}))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;financial-forecasting/at-given-dt-Ft</span>","value":"#'financial-forecasting/at-given-dt-Ft"}
;; <=

;; @@
(take 1 (doquery :smc
  at-given-dt-Ft [Ft-data] ; feed previous frame (Ft) here
  :number-of-particles 1))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:predicts</span>","value":":predicts"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:predicts []]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:result</span>","value":":result"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:do-inc-distrib</span>","value":":do-inc-distrib"},{"type":"html","content":"<span class='clj-double'>0.07219703255576038</span>","value":"0.07219703255576038"}],"value":"[:do-inc-distrib 0.07219703255576038]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:do-not-inc-distrib</span>","value":":do-not-inc-distrib"},{"type":"html","content":"<span class='clj-double'>0.0703930304747511</span>","value":"0.0703930304747511"}],"value":"[:do-not-inc-distrib 0.0703930304747511]"}],"value":"{:do-inc-distrib 0.07219703255576038, :do-not-inc-distrib 0.0703930304747511}"}],"value":"[:result {:do-inc-distrib 0.07219703255576038, :do-not-inc-distrib 0.0703930304747511}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>47.931716376863854</span>","value":"47.931716376863854"}],"value":"[:log-weight 47.931716376863854]"}],"value":"{:predicts [], :result {:do-inc-distrib 0.07219703255576038, :do-not-inc-distrib 0.0703930304747511}, :log-weight 47.931716376863854}"}],"value":"({:predicts [], :result {:do-inc-distrib 0.07219703255576038, :do-not-inc-distrib 0.0703930304747511}, :log-weight 47.931716376863854})"}
;; <=

;; @@
(->> (doquery :smc
  at-given-dt-Ft [Ft-data] ; feed previous frame (Ft) here
  :number-of-particles 100)
     (take 1000)
     :do-inc-distrib
     stat/collect-results
     (stat/empirical-mean))
;; @@

;; @@

;; @@

;; @@

;; @@
