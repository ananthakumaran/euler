;; Surprisingly there are only three numbers that can be written as the
;; sum of fourth powers of their digits:

;; 1634 = 1^4 + 6^4 + 3^4 + 4^4
;; 8208 = 8^4 + 2^4 + 0^4 + 8^4
;; 9474 = 9^4 + 4^4 + 7^4 + 4^4
;; As 1 = 1^4 is not a sum it is not included.

;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.

;; Find the sum of all the numbers that can be written as the sum of
;; fifth powers of their digits.

;; Let say N is the number of digits,
;;         I is the power
;;
;; if (10^N - 1) > (9^I) * N
;; then any number after 10^N can't be written as the sum of I powers of their digits.

(defn max-val [i]
  (loop [n 1]
    (if (> (Math/pow 10 n) (* (Math/pow 9 i) n))
      (* (Math/pow 9 i) n)
      (recur (+ n 1)))))

(defn digits-as-list [n]
  (loop [n n
         r '()]
    (if (> n 9)
      (recur (unchecked-divide n 10)
             (cons (mod n 10) r))
      (cons n r))))

(defn sum [l]
  (reduce #(+ %1 %2)
          l))

(defn sum-of-digit-to-power [n i]
  (sum (map #(Math/pow % i)
            (digits-as-list n))))

(defn problem [i]
  (sum (filter #(= %1 (sum-of-digit-to-power %1 i))
               (range 11 (max-val i)))))

(assert (= (problem 4) 19316))
(assert (= (problem 5) 443839))
