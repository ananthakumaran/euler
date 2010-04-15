;; @author ananthakumaran
;;
;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143 ?


(define largest-prime-factor
  (lambda (n i)
    (cond
     ((eq? 1 n) i)
     ((zero? (modulo n i)) (largest-prime-factor (/ n i) i))
     (else
      (largest-prime-factor n (+ i 1))))))

(largest-prime-factor 600851475143 2)
