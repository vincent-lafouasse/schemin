; 1.1 brain compile some expressions
; not worth writing down

; 1.2 translate an expression

(let ((numerator-term (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
      (divisor-term   (* 3 (- 6 2) (- 2 7))))
  (/ numerator-term divisor-term))

; 1.3 Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers

(let ((sum-of-squares (lambda (a b) (+ (* a a) (* b b)))))
  (sum-of-squares 1 2))
