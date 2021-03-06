;; Each new term in the Fibonacci sequence is generated by adding the previous two terms.
;; By starting with 1 and 2, the first 10 terms will be:
;;
;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;;
;; Find the sum of all the even-valued terms in the sequence which do not exceed four million.

(define fib
  (lambda (a b limit)
    (cond
     ((>= a limit) '())
      (else
       (cons a (fib b (+ a b) limit))))))

(define even
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((zero? (modulo (car lat) 2)) (cons (car lat) (even (cdr lat))))
     (else
      (even (cdr lat))))))

(define sum
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (+ (car lat) (sum (cdr lat)))))))

(define sum-of-fib
  (lambda (limit)
    (sum
     (even
      (fib 1 2 limit)))))