#lang racket

(provide run)

(define (run name fn args)
  (display (string-append "\n\n" name " results:\n"))
  (string-join
    ( map
      (lambda (x)
        (display (list fn x))
        (display " = ")
        (display (fn x))
        (display "\n")
        "-")
      args)))

