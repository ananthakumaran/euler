;; An irrational decimal fraction is created by concatenating the
;; positive integers:

;; 0.123456789101112131415161718192021...

;; It can be seen that the 12th digit of the fractional part is 1.

;; If dn represents the nth digit of the fractional part, find the value
;; of the following expression.

;; d1 d10 d100 d1000 d10000 d100000 d1000000


(define (window n)
  "find the window of the pos and digits
   eg
   (window 443)
   ( 189 -> start-digit-pos
     99 -> start-digit-number
      3 -> digit-size
   )
  "

  (define (iter result pos count n)
    (let ((next-pos (+ pos 
		       (* (* 9 (expt 10 count))
			  (+ count 1))))
	  (next-result (+ result
			  (* 9 (expt 10 count)))))
      (if (>= next-pos n)
	  (list pos result (+ count 1))
	  (iter next-result next-pos  (+ count 1) n))))
  (iter 0 0 0 n))

(define (start-digit w) (cadr w))
(define (start-digit-pos w) (car w))
(define (digit-size w) (caddr w))

(define (digit-at n pos)
  " finds the digit presents at the given position
    in the number "
  (let ((limit (- (expt 10 pos)
		  1)))
    (if (and (> n limit) (not (= 0 pos)))
	(digit-at (quotient n 10) pos)
	(remainder n 10))))

(define (d n)
  "find the digit at the given pos"
  (let ((w (window n)))
    (let ((s (start-digit-pos w))
	  (l (digit-size w))
	  (d (start-digit w)))
      (let ((number (+ d (quotient (- n s) l) 1))
	    (pos (remainder (- n s) l)))
	(if (= 0 pos)
	    (digit-at (- number 1) l)
	    (digit-at number pos))))))


(* (d 1) (d 10) (d 100) (d 1000) (d 10000) (d 100000) (d 1000000))