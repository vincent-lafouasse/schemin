(load "maybe/maybe.scm")
(load "assert.scm")

(assert-eq (maybe/just 3)    (list 'just 3))
(assert-eq (maybe/just "67") (list 'just "67"))
(assert-eq (maybe/nothing)   (list 'nothing))

(assert-eq (maybe/just? (maybe/just 3))    #t)
(assert-eq (maybe/just? (maybe/just "67")) #t)
(assert-eq (maybe/just? (maybe/nothing))   #f)

(assert-eq (maybe/nothing? (maybe/just 3))    #f)
(assert-eq (maybe/nothing? (maybe/just "67")) #f)
(assert-eq (maybe/nothing? (maybe/nothing))   #t)

(assert-eq (maybe/map (maybe/just 5)  (lambda (x) (* x 2))) (maybe/just 10))
(assert-eq (maybe/map (maybe/just 3)  number->string)       (maybe/just "3"))
(assert-eq (maybe/map (maybe/nothing) (lambda (x) (* x 2))) (maybe/nothing))
(assert-eq (maybe/map (maybe/just -4) abs)                  (maybe/just 4))

(let ((chained (maybe/map (maybe/map (maybe/just 3) (lambda (x) (* x 2))) (lambda (x) (+ x 1)))))
  (assert-eq chained (maybe/just 7)))

(display "-- everything ok\n")
