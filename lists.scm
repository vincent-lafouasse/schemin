;;; accessors

(define (first my-list) (car my-list))

(define (last my-list)
  (cond ((null? my-list)       '())
        ((null? (cdr my-list)) (car my-list))
        (else                  (last (cdr my-list)))))

;;; "mutators"
(define (push-front element my-list)
  (cons element my-list))
