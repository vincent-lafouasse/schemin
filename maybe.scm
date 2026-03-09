; an option type

(define (just x) (list 'just x))

(define (nothing) (list 'nothing))

(define (just? maybe) (eq? (car maybe) 'just))

(define (nothing? maybe) (eq? (car maybe) 'nothing))

(define (unwrap maybe)
  (if (just? maybe)
      (cadr maybe)
      (error "tried to unwrap nothing value")))
