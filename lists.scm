;;; accessors

(define (first my-list) (car my-list))

(define (last my-list)
  (cond ((null? my-list)       '())
        ((null? (cdr my-list)) (car my-list))
        (else                  (last (cdr my-list)))))

(define (len my-list)
  (define (iter my-list acc)
    (if (null? my-list)
        acc
        (iter (cdr my-list) (+ 1 acc))))

  (iter my-list 0))

;;; "mutators"

(define (push-front element my-list)
  (cons element my-list))
