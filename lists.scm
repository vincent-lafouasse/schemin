(load "maybe/maybe.scm")

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
(define (list/foldl lst combine init)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (combine acc (car lst)))))

  (iter lst init))

(define (list/sum lst) (foldl lst + 0))

(define (len-also lst) (foldl lst
                              (lambda (acc _) (+ 1 acc))
                              0))

;;; "mutators"

(define (list/push-front element lst)
  (cons element lst))
