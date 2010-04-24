;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;;
;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.


(define (series limit)
  (cond
   ((= 1 limit) 1)
   (else
    (+ (expt limit limit)
       (series (- limit 1))))))

(modulo (series 1000) (expt 10 10))
;; 9110846700