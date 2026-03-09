;;; accessors

(define (first my-list) (car my-list))

(define (last my-list)
  (cond ((null? my-list)       '())
        ((null? (cdr my-list)) (car my-list))
        (else                  (last (cdr my-list)))))

(define (len my-list)
  (if (null? my-list)
      0
      (+ 1 (len (cdr my-list)))))

;;; "mutators"

(define (push-front element my-list)
  (cons element my-list))
