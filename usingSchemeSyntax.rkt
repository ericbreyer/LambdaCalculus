#lang racket
(require "./schemeSyntax.rkt")

(convertChurchList
 (let [
     (fib
      (Y
       (lambda (f)
         (lambda (x)
           ((((ZERO? x) (lambda () ($ 0)))
             (((ZERO? (PRED x)) (lambda () ($ 1)))
              (lambda () ((ADD ((f) (PRED x))) ((f) (PRED (PRED x))))))))))))
     (fibList
      (Y
       (lambda (f)
         (lambda (x)
           ((((ZERO? x) (lambda () ((PAIR (fib ($ 0))) NIL))) (lambda () ((PAIR (fib x)) ((f) (PRED x))))))))))

      ]
  (fibList ($ 16))))


;(let [(THREE ($ 3))
;      (TEN ($ 10))]
;  (ADD THREE TEN))


;(let [
;  (THREE ($ 3))
;  (TEN ($ 10))
;  (ADD3 (ADD THREE))]
;(values
;(convertChurchNumeral THREE)
;(convertChurchNumeral (succ THREE))
;(convertChurchNumeral ((succ) THREE))
;(convertChurchNumeral (ADD THREE TEN))
;(convertChurchNumeral ((ADD THREE) TEN))
;(convertChurchNumeral (((ADD) THREE) TEN))
;(convertChurchNumeral (mult THREE TEN))
;(convertChurchNumeral ((mult THREE) TEN))
;(convertChurchNumeral (((mult) THREE) TEN))
;(convertChurchNumeral (exp THREE TEN))
;(convertChurchNumeral ((exp THREE) TEN))
;(convertChurchNumeral (((exp) THREE) TEN))
;(convertChurchBool True)
;(convertChurchNumeral (True THREE TEN))
;(convertChurchNumeral ((True THREE) TEN))
;(convertChurchNumeral (((True) THREE) TEN))
;(convertChurchBool False)
;(convertChurchNumeral (False THREE TEN))
;(convertChurchNumeral ((False THREE) TEN))
;(convertChurchNumeral (((False) THREE) TEN))
;(convertChurchBool (not True))
;(convertChurchBool ((not) False))
;(convertChurchBool (and True True))
;(convertChurchBool ((and True) False))
;(convertChurchBool (((and) False) True))
;(convertChurchBool (and False False))
;(convertChurchBool (or True True))
;(convertChurchBool ((or True) False))
;(convertChurchBool (((or) False) True))
;(convertChurchBool (or False False))
;(convertChurchPAIR (PAIR THREE TEN))
;(convertChurchNumeral (first (PAIR THREE TEN)))
;(convertChurchNumeral (second ((PAIR THREE) TEN)))
;(convertChurchBool (NIL? NIL))
;(convertChurchBool (NIL? (PAIR THREE TEN)))
;(convertChurchPAIR (Φ (PAIR THREE TEN)))
;(convertChurchNumeral (PRED THREE))
;(convertChurchNumeral (PRED TEN))
;(convertChurchNumeral (sub TEN THREE))
;(convertChurchNumeral (sub THREE TEN))
;(convertChurchNumeral (ADD3 THREE))
;(convertChurchList (list (THREE TEN THREE TEN)))
;)
;)