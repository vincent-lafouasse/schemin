;;;

(define (identity x) x)

(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define pi 3.1415926535)

(define (compose f g) (lambda (x) (f (g x))))

;;;

(define (sum lower upper term next)
  (define (iter lower acc)
    (if (> lower upper)
        acc
        (iter (next lower)
              (+ acc (term lower)))))

  (iter lower 0.0))

(define (integral lower upper f dx)
  ; yes i could factor out the dx multiplication but it makes more sense to me like this
  ; plus i'm doing LISP i don't care this much about performance
  (sum lower
       upper
       (lambda (x) (* (f x) dx)) ; term: infinitesimal rectangle
       (lambda (x) (+ x dx))))   ; next: add dx

;;;

(define pi-approx
  (* 2
     (integral 0
               pi
               (compose square cos)
               0.0001)))

(identity pi-approx)
