#lang s-exp "./schemeSyntax.rkt"

(displayln "natural" (vec-ref 2 (vec-set (vec-lit ($ 1) ($ 2)) ($ 5) 1 2)[1]))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3)) 2 3)))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3) ($ 4)) 3 4)))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3) ($ 4) ($ 5)) 4 5)))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6)) 5 6)))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7)) 6 7)))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7) ($ 8)) 7 8)))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7) ($ 8) ($ 9)) 8 9)))
(displayln "natural" (time (vec-ref (vec-lit ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7) ($ 8) ($ 9) ($ 10)) 9 10)))

(displayln "natural" (time (car ((($ 2) cdr) (list ($ 1) ($ 2) ($ 3))))))
(displayln "natural" (time (car ((($ 3) cdr) (list ($ 1) ($ 2) ($ 3) ($ 4))))))
(displayln "natural" (time (car ((($ 4) cdr) (list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5))))))
(displayln "natural" (time (car ((($ 5) cdr) (list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6))))))
(displayln "natural" (time (car ((($ 6) cdr) (list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7))))))
(displayln "natural" (time (car ((($ 7) cdr) (list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7) ($ 8))))))
(displayln "natural" (time (car ((($ 8) cdr) (list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7) ($ 8) ($ 9))))))
(displayln "natural" (time (car ((($ 9) cdr) (list ($ 1) ($ 2) ($ 3) ($ 4) ($ 5) ($ 6) ($ 7) ($ 8) ($ 9) ($ 10))))))



;(displayln "natural" (let ((a ($ 0)) (b ($ 3)))
;  b))
;
;;
;(displayln "natural list"(list ($ 1) ($ 2) ($ 3)))
;(displayln "natural" ((lambda (func) ((func ($ 2)) ($ 20))) expt))
;(displayln "natural" (/ ($ 8) ($ 3)))
;(displayln "bool" (>= ($ 3) ($ 5)))
;;(LIST )
;;(displayln "natural" (if (zero? ($ 1)) ($ 9)))
;;
;;
;
(displayln "natural list"
  (letrec [
           (fib
            (lambda (x)
              (cond
                ((zero? x) ($ 0))
                ((zero? (sub1 x)) ($ 1))
                (default ((+  (fib (sub1 x))) (fib (sub1 (sub1 x))))))))
           (fibList
            (lambda (x)
              (cond
                [(zero? x) ((cons (fib ($ 0))) null)]
                (default ((cons (fib x)) (fibList (sub1 x)))))))
           (reverse-list
            (lambda (lat)
              (letrec ([revhelper
                        (lambda (x y)
                          (cond
                            ((null? x) y)
                            (default (revhelper (cdr x) (cons (car x) y)))))])
                (revhelper lat null))))
           ]
    (reverse-list (fibList ($ 20)))))
;(displayln "natural list"
;           (letrec [(fact
;                     (lambda (x)
;                       (cond
;                         ((zero? (- x ($ 1))) ($ 1))
;                         (default (* x (fact (sub1 x)))))))
;                    (factList
;                     (lambda (x)
;                       (cond
;                         ((zero? x) ((cons (fact ($ 0))) NIL))
;                         (default ((cons (fact x)) (factList (sub1 x)))))))]
;             (factList ($ 5))
;             ))
;
;(displayln "natural"
;           (letrec [(list-add
;                     (lambda (lat)
;                       (cond
;                         ((null? lat) ($ 0))
;                         (default (+ (car lat) (list-add (cdr lat)))))))]
;             (list-add (list ($ 1) ($ 3) ($ 4)))))
;
;(displayln "signed" (/s (% 17) (% -3)))

(displayln "char" (& #\A))