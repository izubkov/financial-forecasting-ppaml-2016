; E_t  earnings for quarter
;; A_t  amount of dividents paid for quarter

;; - sample parameters that govern decision process
;; for t in 1 to T
;;   - sample current dividents based on
;;       - decision parameters
;;       - previous earnings
;;       - current earnings
;;       - previous dividents

(defn sample*-parameters-from-prior
  (sample* ...))

(defm sample-parameters-from-prior
  (sample ...))

(defn divident-dist
  [cur-earnings prev-earnings prev-dividents model-parameters]
  ...)

(defn simulate-dividents
  [earnings initial-dividents]
  (let [model-parameters (sample*-paramaters-from-prior)]
    (loop [future-earnings (rest earnings)
           prev-earnings (first earnings)
           dividents [initial-dividents]]
      (if (seq future-earnings)
        (let [cur-earnings (first future-earnings)
              prev-dividents (peek dividents)
              cur-dividents (sample*
                              (dividents-dist cur-earnings
                                              prev-earnings
                                              prev-dividents
                                              model-parameters))]
          (recur (rest future-earnings)
                 cur-earnings
                 (conj dividents
                       cur-dividents)))
        {:dividents dividents
         :model-parameters model-parameters}))))

(defquery infer-parameters
  [earnings dividents initial-dividents]
  (let [model-parameters (sample-paramaters-from-prior)]
    (loop [future-earnings (rest earnings)
           future-dividents (rest dividents)
           prev-earnings (first earnings)
           prev-dividents (first dividents)]
      (if (seq future-earnings)
        (let [cur-earnings (first future-earnings)
              cur-dividents (first future-dividents)
              prev-dividents (peek dividents)
              likelihood (dividents-dist cur-earnings
                                         prev-earnings
                                         prev-dividents
                                         model-parameters)]
          (observe liklihood cur-dividents)
          (recur (rest future-earnings)
                 (rest future-dividents)
                 cur-earnings
                 cur-dividents)
          model-parameters)))))
