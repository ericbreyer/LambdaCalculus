#lang s-exp "./schemeSyntax.rkt"

(letrec [
         (stream-add
          (lambda (s1 s2)
              (cons (+ (car s1) (car s2)) 
                    (stream-add (cdr s1) (cdr s2)))))
         (ones
          (cons ($ 1) ones))
         (nats
          (cons ($ 1) (stream-add ones nats)))
         ]
  (displayln "natural" (car (cdr nats))))
