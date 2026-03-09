; we wish to find the sqrt of a real number a
; i.e. solve the equation x^2 = a
; i.e. find the roots of f : x -> x^2 - a. we note that f' : x -> 2x
;
; the Newton method tells us that if x_n is a guess of the value
; then x_{n+1} = x_n - f(x_n)/f'(x_n) is a better guess
; here, f(x)/f'(x) = (x^2 - a) / (2x)

(define (square x) (* x x))

(define (distance x y) (if (> x y) (- x y) (- y x)))

(define (cool-sqrt x)
  (define tolerance 0.000000001)
  (define (close-enough? guess) (< (distance (square guess) x) tolerance))

  (define (next-guess guess)
    (define shift (/ (- (square guess) x)
                     (* 2 guess)))
    (- guess shift))

  (define (iter guess)
    (if (close-enough? guess)
      guess
      (iter (next-guess guess))))

  (iter 1.0))

(define x 2)

(define cool (cool-sqrt x))

cool

(- cool (sqrt x))

(define x (+ 3 1) 4)
