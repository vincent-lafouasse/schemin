(define (isatty?) (not (eq? (output-port-terminal-mode (current-output-port)) 'false)))

(define (colorize color str)
  (if (isatty?)
      (string-append "\033[" color "m" str "\033[0m")
      str))

(define color/red    "31")
(define color/green  "32")
(define color/yellow "33")
