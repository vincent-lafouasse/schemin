(load "maybe/maybe.scm")

; do not come at me for the non standard formatting i do not care
(define-syntax assert-eq
  (syntax-rules () ; following is as many forms as use patterns we recognize. we only recognize 1
    (
      ; define the use pattern
      ; we want 2 arguments. bind them to these names
      (_ expected-expr actual-expr)

      ; when encountering the macro, replace with:
      (
       ; evaluate the expressions once and use them in `let`'s second argument
        let ((expected-value expected-expr) (actual-value actual-expr))

        (if (not (equal? expected-value actual-value))
          (begin
            (display "-- assertion failed")
            (newline)
            (display "--   expected: ")
            (display 'expected-expr) ; the quote means: do not evaluate, just print the S-expression
            (display " => ")
            (display expected-value) ; this is the value
            (newline)
            (display "--   actual:   ")
            (display 'actual-expr)
            (display " => ")
            (display actual-value)
            (newline)
            ; exiting is fine, this is meant for batch mode not interactive mode
            (exit 1)
        ))
      )
    )))

(assert-eq (maybe/just 3)    (list 'just 3))
(assert-eq (maybe/just "67") (list 'just "67"))
(assert-eq (maybe/nothing)   (list 'nothing))

(assert-eq (maybe/just? (maybe/just 3))    #t)
(assert-eq (maybe/just? (maybe/just "67")) #t)
(assert-eq (maybe/just? (maybe/nothing))   #f)

(assert-eq (maybe/nothing? (maybe/just 3))    #f)
(assert-eq (maybe/nothing? (maybe/just "67")) #f)
(assert-eq (maybe/nothing? (maybe/nothing))   #t)

(display "-- everything ok\n")
