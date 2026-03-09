(define (divides n dividor) (= 0 (modulo n dividor)))

; terrible and non extensible, i don't like it either
(define (fizzbuzz n)
  (cond ((divides n 15) "fizzbuzz")
        ((divides n 3)  "fizz")
        ((divides n 5)  "buzz")
        (else           n)))

(define (play n)
  (define (iter i)
    (if (= n i)
        "done"
        (begin
          (display (fizzbuzz i))
          (newline)
          (iter (+ i 1)))))
  (newline)
  (iter 1))

(play 15)
