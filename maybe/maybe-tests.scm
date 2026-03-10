(load "maybe/maybe.scm")

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
          (begin
            (display "-- assertion failed")
            (newline)
            (display "--   expected: ")
            (display 'expected-expr)
            (display " => ")
            (display expected-value)
            (newline)
            (display "--   actual: ")
            (display 'actual-expr)
            (display " => ")
            (display actual-value)
            (newline)
            (error "assertion failed")
        ))
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
