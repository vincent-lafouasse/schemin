(load "assert.scm")

;;;; 1.1 brain compile some expressions
; not worth writing down

;;;; 1.2 translate an expression

(define this-expression (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
                           (* 3 (- 6 2) (- 2 7))))

(assert-eq this-expression (- (/ 37 150)))


;;;; 1.3 Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers

(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

; if tasked to do the same with a list i would probably sort it first
; for 3 hardcoded arguments this is fine
(define (sum-of-squares-larger a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c)) ; a is min
        ((< b c)               (sum-of-squares a c)) ; b is min
        (else                  (sum-of-squares a b)) ; c is min
        ))

(assert-eq (sum-of-squares-larger -67 -1 1) 2)
(assert-eq (sum-of-squares-larger -1 -67 1) 2)
(assert-eq (sum-of-squares-larger -1 1 -67) 2)

;;;; 1.4 describe the following expression

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; we either addition or subtract b from a depending on the sign of b
; if b is positive, this evaluates to a + b ie a + abs(b)
; else            , this evaluates to a - b ie a + abs(b)
;
; this is indeed a-plus-abs-b

;;;; 1.5 applicative-order vs normal-order
; in each case what happens if we do:

(define (p) (p))  ; infinite loop
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

; > Scheme is an applicative-order language, namely, that all the arguments to
; > Scheme procedures are evaluated when the procedure is applied. In contrast,
; > normal-order languages delay evaluation of procedure arguments until the
; > actual argument values are needed.

; in normal order the arguments to procedures are lazily evaluated while in
; applicative order. they are eagerly evaluated
;
; here in particular, (p) can never be evaluated but it doesn't have to be

; so in normal order,
(test 0 (p))       ; test is expanded without ever evaluating its arguments
(if (= 0 0) 0 (p)) ; the (p) branch is not taken
0                  ; (p) is never evaluated

; (p) never has to be evaluated

; in applicative order the arguments to test are always evaluated
; equivalently to

(let ((x 0)
      (y (p))) ; hangs
  (test x y))  ; will never happen
