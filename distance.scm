(define (square x) (* x x))

(define (sum-squares x y)
  (+ (square x)
     (square y)))

(define (norm x y) (sqrt (sum-squares x y)))

(define (distance x1 y1 x2 y2)
  (norm (- x1 x2)
        (- y1 y2)))

(distance 0 0 1 1)
