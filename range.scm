(load "maybe/maybe.scm")

(define (range-iterator start maybe-end)
  ; (list next '(start maybe-end))
  (define next
    (lambda ()
      (if (and (just? maybe-end)
               (>= start (maybe/unwrap maybe-end)))
          (list something?? (nothing))
          (list (range-iterator (+ 1 start) maybe-end)
                (just start)))
      ))
  )
