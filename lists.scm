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
(define (list/fold-left combine init lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (combine acc (car lst)))))

  (iter lst init))

(define (list/reverse lst) (list/fold-left (lambda (acc e) (cons e acc)) '() lst))

(define (list/sum lst) (fold-left + 0 lst))

(define (len-also lst) (fold-left (lambda (acc _) (+ 1 acc))
                                  0
                                  lst))

;;; "mutators"

(define (list/push-front element lst)
  (cons element lst))
