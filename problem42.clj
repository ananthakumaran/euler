; The nth term of the sequence of triangle numbers is given by, tn =
; ½n(n+1); so the first ten triangle numbers are:

; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

; By converting each letter in a word to a number corresponding to its
; alphabetical position and adding these values we form a word
; value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
; t10. If the word value is a triangle number then we shall call the
; word a triangle word.

; Using words.txt (right click and 'Save Link/Target As...'), a 16K text
; file containing nearly two-thousand common English words, how many are
; triangle words?

(use 'clojure.contrib.string)

(defn includes? [coll x]
  (boolean (some (fn [y] (= y x)) coll)))
(defn sum [coll]
  (reduce #(+ %1 %2) coll))


(def *triangles*
     (map (fn [n] (/ (* n (+ n 1)) 2)) (range 1 100)))

(defn triangle? [x]
  (includes? *triangles* x))

(let [words (filter #(not (= "" %))
		    (split #"\",\"|\n|\"" (slurp "42.txt")))]
  (count (filter
	  #(triangle? %)
	  (map
	   (fn [word]
	     (sum (map #(- (int %) 64) word)))
	   *words*))))
; 162
