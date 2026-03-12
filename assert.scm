(load "color.scm")

; do not come at me for the non standard formatting i do not care
(define-syntax assert-eq
  (syntax-rules () ; following is as many forms as use patterns we recognize. we only recognize 1
    (
      ; define the use pattern
      ; we want 2 arguments. bind them to these names
      (_ actual-expr expected-expr)

      ; when encountering the macro, replace with:
      (
       ; evaluate the expressions once and use them in `let`'s second argument
        let ((expected-value expected-expr) (actual-value actual-expr))

        (if (not (equal? expected-value actual-value))
            (begin
              (display (colorize color/red "-- assertion failed"))
              (newline)
              (display (colorize color/green "--   expected: "))
              (display 'expected-expr) ; the quote means: do not evaluate, just print the S-expression
              (display (colorize color/yellow " => "))
              (display expected-value) ; this is the value
              (newline)
              (display (colorize color/red "--   actual:   "))
              (display 'actual-expr)
              (display (colorize color/yellow " => "))
              (display actual-value)
              (newline)
              ; exiting is fine, this is meant for batch mode not interactive mode
              (exit 1)
            ))
      )
    )))
