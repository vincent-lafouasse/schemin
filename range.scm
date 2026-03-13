(load "maybe/maybe.scm")

; we'll try and make an OCaml sequence
; an OCaml sequence is a thunk that optionally returns a value and the next
; iterator

(define (range start maybe-end)
  (lambda ()
    (if (and (maybe/just? maybe-end)
             (>= start (maybe/unwrap maybe-end)))
        (maybe/nothing)
        (maybe/just (cons start (range (+ 1 start) maybe-end)))
      )))

(define it (range 0 (maybe/just 3)))

(it) ; => (just (0 . #[compound-procedure 3]))
