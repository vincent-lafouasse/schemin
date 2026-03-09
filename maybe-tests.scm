(load "maybe.scm")

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
(assert-eq (nothing? (nothing))   #f)
