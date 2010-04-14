;; @author ananthakumaran
;;
;; 2520 is the smallest number that can be divided by each of the numbers 
;; from 1 to 10 without any remainder.
;;
;; What is the smallest number that is evenly divisible by all of 
;; the numbers from 1 to 20?

(define gcd
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else
      (gcd b (modulo a b))))))

(define lcm
  (lambda (a b)
    (/ (* a b) (gcd a b))))

(define problem
  (lambda (limit n)
    (cond
     ((eq? 1 limit) n)
     (else
      (problem (- limit 1) (lcm n limit))))))

(problem 20 1)
      


