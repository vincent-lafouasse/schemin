(load "maybe/maybe.scm")
(load "assert.scm")

;;; accessors

(define (list/first lst)
  (if (null? lst)
       (maybe/nothing)
       (maybe/just (car lst))))

(define (list/last lst)
  (cond ((null? lst)       (maybe/nothing))
        ((null? (cdr lst)) (maybe/just (car lst)))
        (else              (list/last (cdr lst)))))

(define (list/len lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (+ 1 acc))))

  (iter lst 0))

; `combine` is a binary operator that combines the accumulator with the current element
; next-accumulator = (combine accumulator element)
(define (list/fold-left combine init lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (combine acc (car lst)))))

  (iter lst init))

(define (list/reverse lst) (list/fold-left (lambda (acc e) (cons e acc)) '() lst))

(define (list/sum lst) (list/fold-left + 0 lst))

(define (list/len-also lst) (list/fold-left (lambda (acc _) (+ 1 acc))
                                            0
                                            lst))

;;; "mutators"

(define (list/push-front element lst)
  (cons element lst))

;;; tests

(define (list/tests) (begin
  (assert-eq (list/first '())    (maybe/nothing))
  (assert-eq (list/first '(67))  (maybe/just 67))
  (assert-eq (list/first '(1 2)) (maybe/just 1))

  (assert-eq (list/last '())    (maybe/nothing))
  (assert-eq (list/last '(67))  (maybe/just 67))
  (assert-eq (list/last '(1 2)) (maybe/just 2))

  (assert-eq (list/len '())    0)
  (assert-eq (list/len '(67))  1)
  (assert-eq (list/len '(1 2)) 2)

  (assert-eq (list/len-also '())    0)
  (assert-eq (list/len-also '(67))  1)
  (assert-eq (list/len-also '(1 2)) 2)

  (assert-eq (list/sum '())    0)
  (assert-eq (list/sum '(67))  67)
  (assert-eq (list/sum '(1 2)) 3)

  (display "-- everything ok\n")
))

(list/tests)
