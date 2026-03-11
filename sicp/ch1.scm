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
(define (p) (error "don't do that")) ; actually don't kill my REPL please

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
;
; we can note that macros substitute their arguments without evaluating them
; (that's the whole point)
; so macros have normal-order evaluation

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

;;;; 1.6 could we just rewrite `if` by wrapping a `cond` in a procedure ?

(define (terrible-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; no the problem is that expressions have to pass through the terrible-if procedure
; and hence will be eagerly evaluated

(define (destroy-the-world) (error "you shall not pass"))

(if #t 'ok (destroy-the-world))          ; this is fine
(terrible-if #t 'ok (destroy-the-world)) ; this isn't

; this would work tho. the cond does not evaluate branchs not taken and the
; macro ensures everything is passed to cond as if we called it ourselves
(define-syntax cool-if
  (syntax-rules ()
    ((_ pred? then-clause else-clause)
     (cond (pred? then-clause)
           (else else-clause)))))

; or pass a thunk. thunks can be evaluated that's fine there're just functions
; we just make sure to not call it, which the cond should take care of without fuss
(define (coolest-if pred? then-thunk else-thunk)
  (cond (pred? (then-thunk))
        (else  (else-thunk))))

; the world is safe
(cool-if    #t 'ok (destroy-the-world))
(coolest-if #t
            (lambda () 'ok)
            (lambda () (destroy-the-world)))

;;;; 1.7 why is this terrible ?

(define (close-enough? x y) (< (abs (- x y)) 0.00001))

; we have a problem because a fixed tolerance falls apart for arbitrary small
; numbers as the ratio value/tolerance starts getting too small.
;
; in a world of infinite precision (e.g. non QM physics) the tolerance should scale with the values.
;
; in the IEEE world, a tolerance of (next 0.0) is the best we can get for small numbers (but way too drastic)
; however even with real world floats we still have problems for large numbers.
;
; without scaling, a tolerance of 0.000001 can be literally impossible to reach
; as the distance between 2 successive floats gets bigger
; next(1000000000.0) - 1000000000.0 is many many orders of magnitude larger than the (next 0.0)

; the answer is to not constrain dX to a small number but dX/X
(define (distance x y) (abs (- x y)))

; 1:1000 should be plenty close enough. i've done way worse chemistry and i dare
; you to measure 5mg of powder within 1:1000
(define tolerance 0.001)

(define (close-enough? x y)
  (< (/ (distance x y) (abs x))
     tolerance))
; we could divide by x or y, it shouldn't matter if they're close
; a more principled approach would probably average them in some way
; and we could probably add an epsilon to (abs x) to avoid division by 0
;
; the recommended implementation is usually to use a fixed epsilon for small
; floats, and a scaling epsilon for large floats
