;; In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
;;
;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;; It is possible to make £2 in the following way:
;;
;; 1£1 + 150p + 220p + 15p + 12p + 31p
;; How many different ways can £2 be made using any number of coins?


(define (count-change amount n)
  (cond
   ((= 0 amount) 1)
   ((or (< amount 0) (= 0 n)) 0)
   (else
    (+ (count-change amount (- n 1))
       (count-change (- amount (value n)) n)))))

(define (value n)
  (cond
   ((= 1 n) 1)
   ((= 2 n) 2)
   ((= 3 n) 5)
   ((= 4 n) 10)
   ((= 5 n) 20)
   ((= 6 n) 50)
   ((= 7 n) 100)
   ((= 8 n) 200)))

(count-change 200 8)

;; 73682