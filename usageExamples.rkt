#lang s-exp "./schemeSyntax.rkt"

#;(displaylnAuto!
 (letrec ([fib (tacit-cond
                ((SR or (= ($ 1)) (= ($ 0))) I)
                ((K else) (SR + (o fib (C - ($ 1))) (o fib (C - ($ 2))))))])
   (fib ($ 20))))


(displaylnAuto!
 (with [define test ($$ 2)]
       [define test2 (*s test test)]
       do
       test2))

(letrec ([sieve
          (lambda (stream)
            (cons/l (car/l stream)
                         (sieve (filter/l
                                 (lambda (x) (not (zero? (% x (car/l stream)))))
                                 (cdr/l stream)))))]
         [colatz
          (lambda (num)
            (letrec ([helper (lambda (n steps)
                               (cond
                                 ((= n ($ 1)) steps)
                                 ((= (% n ($ 2)) ($ 1)) (helper (add1 (* n ($ 3))) (add1 steps)))
                                 (default (helper (/ n ($ 2)) (add1 steps)))))])
              (helper num ($ 0))))]

         [ones         (cons/l ($ 1) ones)]
         [twos         (cons/l ($ 2) twos)]
         [nats         (cons/l ($ 1) ((combine/l +) ones nats))]
         [powsOf2      (cons/l ($ 1) ((combine/l *) twos powsOf2))]
         [squares      ((combine/l *) nats nats)]
         [fibs         (cons/l ($ 0) (cons/l ($ 1) ((combine/l +) fibs (cdr/l fibs))))]
         #;[facts        (cons/l ($ 1) ((combine/l *) facts nats))]
         [odds         (filter/l (lambda (x) (= (% x ($ 2)) ($ 1))) nats)]
         [primes       (sieve (cdr/l nats))]
         [mapSquares   (map/l (lambda (x) (* x x)) nats)]
         [cows         (cons/l ($ 1) (cons/l ($ 1) (cons/l ($ 1) ((combine/l +) cows (cdr/l (cdr/l cows))))))]
         [rones        (cons/l ($$$ 1) rones)]
         [rnats        (cons/l ($$$ 1) ((combine/l +r) rones rnats))]
         [rfibs        (cons/l ($$$ 0) (cons/l ($$$ 1) ((combine/l +r) rfibs (cdr/l rfibs))))]
         [phiEstimate  ((combine/l /r) (cdr/l (cdr/l rfibs)) (cdr/l rfibs))]
         [colatzs      (map/l colatz nats)]
         #;[acc          ($ 50)]
         #;[primeDist    ((*o
                         ((combine/l -) (cdr/l primes))
                         (limit/l acc)
                         (map/l (*o nat->signed /1))
                         (reduce/l (*o (C /r ((*o nat->signed /1) acc)) +r) ($$$ 0))
                         )
                        primes)]
         #;[pCounting   ((map/l (*o (C limit/l (cdr/l nats)) sieve (map/l (K ($ 1))) (reduce/l + ($ 0))) nats))]
         )
  (previewStream! ($ 10) ones)
  (previewStream! ($ 10) twos)
  (previewStream! ($ 10) nats)
  (previewStream! ($ 10) powsOf2)
  (previewStream! ($ 10) squares)
  (previewStream! ($ 10) fibs)
  #;(previewStream ($ 10) facts)
  (previewStream! ($ 10) odds)
  (previewStream! ($ 10) primes)
  #;(previewStream (map/l (filter/l (lambda (x) (= (% x ($ 10)) ($ 3))) primes) (lambda (x) (/ x ($ 10)))) ($ 10))
  (previewStream! ($ 10) cows)
  ;(displaylnAuto! (car/l (cdr/l (cdr/l rfibs))))
  (previewStream! ($ 10) rnats)
  (previewStream! ($ 10) phiEstimate)
  (previewStream! ($ 10) colatzs)
  #;(previewStream! ($ 10) ((*o (filter/l (lambda (x) ((o* zero? (C % ($ 2))) x))) (map/l (lambda (x) (* x x)))) nats))
  #;(displaylnAuto! ((*o (limit/l ($ 10)) (reduce/l + ($ 0))) nats))
  #;(previewStream! ($ 10) pCounting)
  ;(displaylnAuto! primeDist)
  ;(previewStream! ($ 10) primeDist)
  )