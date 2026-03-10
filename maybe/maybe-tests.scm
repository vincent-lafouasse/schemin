(load "maybe/maybe.scm")

(define (assert-eq expected actual)
  (if (not (equal? expected actual))
      (error "assertion failed: expected " expected 'but 'got actual)))

; do not come at me for the non standard formatting i do not care
(define-syntax assert-eq
  (syntax-rules ()
    ( ; the first and only use pattern recognized
      (_ expected-expr actual-expr) ; we want 2 arguments. bind them to these names

      ; when encountering the macro, replace with:
      (
       ; evaluate the expressions once and use them in `let`'s second argument
        let ((expected-value expected-expr) (actual-value actual-expr))

        (if (not (equal? expected-value actual-value))
            (error "something use both 'expr and value")
        )
      )
    )))

(assert-eq (just 3)    (list 'just 3))
(assert-eq (just "67") (list 'just "67"))
(assert-eq (nothing)   (list 'nothing))

(assert-eq (just? (just 3))    #t)
(assert-eq (just? (just "67")) #t)
(assert-eq (just? (nothing))   #f)

(assert-eq (nothing? (just 3))    #f)
(assert-eq (nothing? (just "67")) #f)
(assert-eq (nothing? (nothing))   #f)
