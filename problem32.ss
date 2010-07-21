;;We shall say that an n-digit number is pandigital if it makes use of all the digits 1
;; to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
;;
;;The product 7254 is unusual, as the identity, 39 * 186 = 7254,
;; containing multiplicand, multiplier, and product is 1 through 9 pandigital.
;;
;;Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
;;

;; ans 45228
(define (pandigital lat)
  (define (pandigital-iter lat)
    (cond
     ((null? lat) 0)
     (else 
      (+ (paner (car lat)) (pandigital-iter (cdr lat))))))
  (pandigital-iter (permutate lat)))
  



(define (paner lat)


(define (series lat)
" build a triangle series of a number 
  eg (1 2) yields ((1)(1 2))"
  (define (series-iter lat count i)
    (cond
     ((eq? (+ count 1) i) '())
     (else
      (cons (take lat i) (series-iter lat count (+ i 1))))))
  (series-iter lat (length lat) 1))
  
(define (permutate lat)
  (cond
   ((null? (cdr lat)) (cons lat '()))
   (else
    (insert (permutate (cdr lat)) (car lat)))))

(define (insert lat item)
" inserts the item in all the possilble places in the list in lat
  eg: (insert '((1) (2)) 3) will yield ((1 3) (3 1) (2 3) (3 2)) "
  (define (insert-recur lat item n)
    (cond
     ((zero? n) (cons (add lat item 0) '()))
     (else
      (cons (add lat item n) (insert-recur lat item (- n 1))))))
  (cond 
   ((null? lat) '())
   (else 
    (build (insert-recur (car lat) item (length (car lat))) (insert (cdr lat) item)))))

;; helpers 

(define (build left right)
  (cond
   ((null? (cdr left)) (cons (car left) right))
   (else
    (cons (car left) (build (cdr left) right)))))

(define (add lat item pos)
  (cond
   ((zero? pos) (cons item lat))
   (else
    (cons (car lat) (add (cdr lat) item (- pos 1)))))) 

(define (take lat n)
  (cond
   ((zero? n) '())
   (else
    (cons (car lat) (take (cdr lat) (- n 1))))))

;; trim helpers
(define (trim-last-two lat)
  (trim-last-two lat (lambda (lat) (null? (cdr (cdr lat))))))

(define (trim-first-two lat)
  (cdr (cdr lat)))

(define (trim-first-and-last lat)
  (trim-last (cdr lat) (lambda (lat) (null? (cdr lat)))))

(define (trim-last lat condition)
  (if (condition lat)
      '()
      (cons (car lat) (trim-last (cdr lat) condition))))
