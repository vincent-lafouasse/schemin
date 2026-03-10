(load "assert.scm")

; 1.1 brain compile some expressions
; not worth writing down

; 1.2 translate an expression

(let ((numerator-term (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
      (divisor-term   (* 3 (- 6 2) (- 2 7))))
  (/ numerator-term divisor-term))

; 1.3 Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers

(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(define (sum-of-squares-larger a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c)) ; a is min
        ((< b c)               (sum-of-squares a c)) ; b is min
        (else                  (sum-of-squares a b)) ; c is min
        ))

(assert-eq (sum-of-squares-larger -67 -1 1) 2)
(assert-eq (sum-of-squares-larger -1 -67 1) 2)
(assert-eq (sum-of-squares-larger -1 1 -67) 2)
