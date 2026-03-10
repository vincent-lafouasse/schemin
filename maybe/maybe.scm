; an option type

(define (maybe/just x) (list 'just x))

(define (maybe/nothing) (list 'nothing))

(define (maybe/just? maybe) (eq? (car maybe) 'just))

(define (maybe/nothing? maybe) (eq? (car maybe) 'nothing))

(define (maybe/unwrap maybe)
  (if (maybe/just? maybe)
      (cadr maybe)
      (error "tried to unwrap nothing value")))

(define (maybe/unwrap-or maybe default)
  (if (maybe/just? maybe)
      (maybe/unwrap maybe)
      default))

(define (maybe/map maybe f)
  (if (maybe/just? maybe)
      (maybe/just (f (maybe/unwrap maybe)))
      (maybe/nothing)))


(define (maybe/filter maybe pred?)
  (cond ((maybe/nothing? maybe)       (maybe/nothing))
        ((pred? (maybe/unwrap maybe)) maybe)
        (else                         (maybe/nothing))))
