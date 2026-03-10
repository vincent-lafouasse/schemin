(define (isatty?) (not (eq? (output-port-terminal-mode (current-output-port)) 'false)))

(isatty?)
