(define (isatty?) (not (eq? (output-port-terminal-mode (current-output-port)) 'false)))

(define (colorize color str)
  (if (isatty?)
      (string-append "\033[" color "m" str "\033[0m")
      str))

(define red    "31")
(define green  "32")
(define yellow "33")

(display (colorize green "✓ all tests passed"))
