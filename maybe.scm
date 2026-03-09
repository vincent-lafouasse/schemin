; an option type

(define (just x) (list 'just x))

(define (nothing) (list 'nothing))

(define (just? maybe) (eq? (car maybe) 'just))

(define (nothing? maybe) (eq? (car maybe) 'nothing))

(define (unwrap maybe)
  (if (just? maybe)
      (cadr maybe)
      (error "tried to unwrap nothing value")))

(define (unwrap-or maybe default)
  (if (just? maybe)
      (unwrap maybe)
      default))

(define (maybe-map maybe f)
  (if (just? maybe)
      (f (unwrap maybe))
      (nothing)))


(define (maybe-filter maybe pred?)
  (cond ((nothing? maybe)       (nothing))
        ((pred? (unwrap maybe)) maybe)
        (else                   (nothing))))

;; tests

(define (assert-eq expected actual)
  (if (not (equal? expected actual))
      (error "assertion failed: expected " expected 'but 'got actual)))

(assert-eq (just 3)    (list 'just 3))
(assert-eq (just "67") (list 'just "67"))
(assert-eq (nothing)   (list 'nothing))

(assert-eq (just? (just 3))    #t)
(assert-eq (just? (just "67")) #t)
(assert-eq (just? (nothing))   #f)

(assert-eq (nothing? (just 3))    #f)
(assert-eq (nothing? (just "67")) #f)
(assert-eq (nothing? (nothing))   #t)
