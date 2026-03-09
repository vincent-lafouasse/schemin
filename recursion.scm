(define (dec n) (- n 1))

(define (bad-triangle n)
  (if (= n 0)
      0
      (+ n (bad-triangle (dec n)))))

; blow up the stack
(bad-triangle 10000000)

(define (good-triangle n)
  (define (iter n acc)
    (if (= n 0)
        acc
        (iter (dec n)
              (+ acc n))))
  (iter n 0))

; fine but might take a second
(good-triangle 10000000)
