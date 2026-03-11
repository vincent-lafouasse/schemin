(load "assert.scm")

; ---- 1. Building Abstractions with Procedures

; ---- 1.1 The Elements of Programming

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
;
; we note that procedures (which + and - are) are just values and we can swap
; them around as we would integers

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
(define (relative-distance x y reference)
  (/ (distance x y)
     (abs reference)))

; 1:1000 should be plenty close enough. i've done way worse chemistry and i dare
; you to measure 5mg of powder within 1:1000
(define tolerance 0.001)

(define (close-enough? x y)
  (< (relative-distance x y x)
     tolerance))
; we could use x or y as reference, it shouldn't matter if they're close
; a more principled approach would probably average them in some way and we could
; probably add an epsilon to the reference to avoid division by 0
;
; the recommended implementation is usually to use a fixed epsilon for small
; floats, and a scaling epsilon for large floats
;
; here's a C++ implementation from JUCE that illustrates this
#|
constexpr bool approximatelyEqual (Type a, Type b,
                                   Tolerance<Type> tolerance = Tolerance<Type>{}
                                        .withAbsolute (std::numeric_limits<Type>::min())
                                        .withRelative (std::numeric_limits<Type>::epsilon()))
{
    if (! (juce_isfinite (a) && juce_isfinite (b)))
        return exactlyEqual (a, b);

    const auto diff = std::abs (a - b);

    return diff <= tolerance.getAbsolute()
        || diff <= tolerance.getRelative() * std::max (std::abs (a), std::abs (b));
}
|#

;;;; 1.8 Newton but for cube roots
; > Newton's method for cube roots is based on the fact that if y is an
; > approximation to the cube root of x, then a better approximation is given by
; > the value (x/y^2 + 2y)/3.
; >
; > Use this formula to implement a cube-root procedure analogous to the
; > square-root procedure.

(define (better-guess x guess)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

; we'll reuse this, we just won't pass 0
(define (close-enough? x y)
  (< (relative-distance x y x)
     0.001))

(define (cube-root x)
  (define (iter guess)
    (if (close-enough? x (* guess guess guess))
        guess
        (iter (better-guess x guess))))

  (iter 1.0))

(cube-root 27.0) ; => 3.0000005410641766

; because why not, let's count the number of iterations it took to get there
(define (cube-root-monitored x)
  (define (iter guess iteration)
    (if (close-enough? x (* guess guess guess))
        (list guess iteration)
        (iter (better-guess x guess) (+ 1 iteration))))

  (iter 1.0 0))

(cube-root-monitored 27.0) ; => (3.0000005410641766 7)
; pretty nice

; ---- 1.2 Procedures and the Processes They Generate

;;;; 1.9 recursive vs iterative addition
; > Using the substitution model, illustrate the process generated by each
; > procedure in evaluating (+ 4 5). Are these processes iterative or recursive ?

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (add1 a b)
  (if (= a 0)
      b
      (inc (add1 (dec a) b))))

(define (add2 a b)
  (if (= a 0)
      b
      (add2 (dec a) (inc b))))

; my first instinct is that add1 is not tail recursive. hence when doing (+ 1
; (add1 ...)) it has to hold the value 1 in the stack frame until add1 is done.
; In contrast, in add2 the tail call is the recursive calls. because of eager
; evaluation, its operands will be evaluated first, leaving nothing in the
; stack frame of use.

; let's apply the substitution model
(add1 4 5)

(if (= 4 0)
    5
    (inc (add1 (dec 4) 5)))

(inc (add1 (dec 4) 5))

(inc (add1 3 5)) ; the inc stays up

(inc (if (= 3 0) ....)) ; the inc stays up

(inc (inc (add1 2 5)))
(inc (inc (inc (add1 1 5))))
(inc (inc (inc (inc (add1 0 5))))) ; base case
(inc (inc (inc (inc 5))))
9

; the tower of incs is a direct representation of the stack frames building up
; and down. this is a recursive process. put anything large in the first
; argument and you will blow up your stack

(add2 4 5)
(if (= 4 0)
    5
    (add2 (dec 4) (inc 5)))

(add2 (dec 4) (inc 5))

(add2 3 6) ; no pending inc, we start fresh
(add2 (dec 3) (inc 6))

(add2 2 7)
(add2 (dec 2) (inc 7))

(add2 1 8)
(add2 (dec 1) (inc 8))

(add2 0 9) ; base case
9

; no pending stack frames. this process is iterative
; with guaranteed TCO go nuts and add large numbers (but maybe be prepared to wait)

;;;; 1.10 Ackermann's function and what it can compute (supposedly a lot)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; > What are the values of the following expressions ?
(A 1 10)
(A 2 4)
(A 3 3)

; alright time to brain compile, looking out for any of them equalling 0, and for y equalling 1

(A 1 10)
(A (- 1 1) (A 1 (- 10 1)))
(A 0 (A 1 9)) ; base case

(* 2 (A 1 9))

(* 2 (A (- 1 1) (A 1 (- 9 1))))
(* 2 (A 0 (A 1 8)))
(* 4 (A 1 8))

; ok i see the pattern
(* (pow 2 2) (A 1 8))
(* (pow 2 3) (A 1 7))
(* (pow 2 4) (A 1 6))
(* (pow 2 5) (A 1 5))
(* (pow 2 6) (A 1 4))
(* (pow 2 7) (A 1 3))
(* (pow 2 8) (A 1 2))
(* (pow 2 9) (A 1 1)) ; base case

(* (pow 2 9) 2)
(pow 2 10) ; 1024 as expected

; so (A 1 n) is 2^n

; let's keep going
(A 2 4)
(A 1 (A 2 3))

; since (A 1 n) is (pow 2 n)

(pow 2 (A 2 3))
(pow 2 (pow 2 (A 2 2)))
(pow 2 (pow 2 (pow 2 (A 2 1)))) ; base case

(pow 2 (pow 2 (pow 2 2)))

; that's 2^16 ie 64Ki

; so (A 2 n) is 2^2^...^2 with n 2's in there
; i.e. 2↑↑4 (tetration)

; so finally to answer the question
; (A 0 n) = 2n,   i.e. 2+2+...+2 with n 2's in there
; (A 1 n) = 2^n,  i.e. 2*2*...*2 with n 2's in there
; (A 2 n) = 2↑↑n, i.e. 2^2^...^2 with n 2's in there
