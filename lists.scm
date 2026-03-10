(load "maybe/maybe.scm")

;;; accessors

(define (first my-list)
  (if (null? my-list)
       (maybe/nothing)
       (maybe/just (car my-list))))

(define (last my-list)
  (cond ((null? my-list)       (maybe/nothing))
        ((null? (cdr my-list)) (maybe/just (car my-list)))
        (else                  (last (cdr my-list)))))

(define (len my-list)
  (define (iter my-list acc)
    (if (null? my-list)
        acc
        (iter (cdr my-list) (+ 1 acc))))

  (iter my-list 0))

; `combine` is a binary operator that combines the accumulator with the current element
; next-accumulator = (combine accumulator element)
(define (foldl my-list combine init)
  (define (iter my-list acc)
    (if (null? my-list)
        acc
        (iter (cdr my-list) (combine acc (car my-list)))))

  (iter my-list init))

(define (sum-list my-list) (foldl my-list + 0))

(define (len-also my-list) (foldl my-list
                                  (lambda (acc _) (+ 1 acc))
                                  0))

;;; "mutators"

(define (push-front element my-list)
  (cons element my-list))
