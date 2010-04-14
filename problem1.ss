;; If we list all the natural numbers below 10 that are multiples of 3 or 5,
;; we get 3, 5, 6 and 9. The sum of these multiples is 23.
;; Find the sum of all the multiples of 3 or 5 below 1000.

(define sum-of-multiples
  (lambda (n)
    (cond
     ((zero? n) 0)
     ((or (zero? (modulo n 3)) (zero? (modulo n 5))) (+ n (sum-of-multiples (- n 1))))
     (else
      (sum-of-multiples (- n 1))))))