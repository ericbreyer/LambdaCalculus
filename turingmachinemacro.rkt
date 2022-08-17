#lang racket
(require "./schemeSyntax.rkt")

(provide turing-machine universal-turing-machine turing-machine/log universal-turing-machine/log)

(define-syntax (turing-machine stx)
  (syntax-parse stx
    [(_ tape-start
        start-state
        (s n s-prime n-prime dir)...)
     #'(letrec [
                (reverse
                 (lambda (lat rev)
                   (cond
                     ((null? lat) rev)
                     (default (reverse (cdr lat) (cons (car lat) rev))))))
                (blanks
                 (cons (& #\_) blanks))
                (start-tape
                 (lambda (starting)
                   (letrec [(helper
                             (lambda (lat)
                               (cond
                                 ((null? lat) blanks)
                                 (default (cons (car lat) (helper (cdr lat)))))))]
                     (cons blanks (helper starting)))))
                (move-tape
                 (lambda (d tape)
                   (cond
                     ((= d ($ 0)) (cons (cdr (car tape)) (cons (car (car tape)) (cdr tape))))
                     ((= d ($ 1)) (cons (cons (car (cdr tape)) (car tape)) (cdr (cdr tape))))
                     ((= d ($ 2)) tape))))
                (turing-step
                 (lambda (tape state)
                   (cond
                     ((and (= state s) (= (car (cdr tape)) n)) (turing-step (move-tape dir (cons (car tape) (cons n-prime (cdr (cdr tape))))) s-prime))...
                     (else (cons tape state)))))]
         (turing-step (start-tape tape-start) start-state))]))

(define-syntax (universal-turing-machine stx)
  (syntax-parse stx
    [(_)
     #'(letrec [
                (reverse
                 (lambda (lat rev)
                   (cond
                     ((null? lat) rev)
                     (default (reverse (cdr lat) (cons (car lat) rev))))))
                (blanks
                 (cons (& #\_) blanks))
                (start-tape
                 (lambda (starting)
                   (letrec [(helper
                             (lambda (lat)
                               (cond
                                 ((null? lat) blanks)
                                 (default (cons (car lat) (helper (cdr lat)))))))]
                     (cons blanks (helper starting)))))
                (move-tape
                 (lambda (d tape)
                   (cond
                     ((= d ($ 0)) (cons (cdr (car tape)) (cons (car (car tape)) (cdr tape))))
                     ((= d ($ 1)) (cons (cons (car (cdr tape)) (car tape)) (cdr (cdr tape))))
                     ((= d ($ 2)) tape))))
                (turing-step
                 (lambda (tape state transitions)
                   (letrec [(parse-transitions
                             (lambda (t)
                               (cond
                                 ((null? t) (cons tape state))
                                 ((and (= state (car (car t))) (= (car (cdr tape)) (car (cdr (car t)))))
                                  (turing-step (move-tape (car (cdr (cdr (cdr (cdr (car t)))))) (cons (car tape) (cons  (car (cdr (cdr (cdr (car t))))) (cdr (cdr tape))))) (car (cdr (cdr (car t)))) transitions))
                                 (else (parse-transitions (cdr t))))))]
                     (parse-transitions transitions))))]
                                                   
(lambda (tape-start) (lambda (start-state) (lambda (transitions) (turing-step (start-tape tape-start) start-state transitions)))))]))

(define-syntax (turing-machine/log stx)
  (syntax-parse stx
    [(_ tape-start
        start-state
        (s n s-prime n-prime dir)...)
     #'(letrec [
                (reverse
                 (lambda (lat rev)
                   (cond
                     ((null? lat) rev)
                     (default (reverse (cdr lat) (cons (car lat) rev))))))
                (blanks
                 (cons (& #\_) blanks))
                (start-tape
                 (lambda (starting)
                   (letrec [(helper
                             (lambda (lat)
                               (cond
                                 ((null? lat) blanks)
                                 (default (cons (car lat) (helper (cdr lat)))))))]
                     (cons blanks (helper starting)))))
                (move-tape
                 (lambda (d tape)
                   (cond
                     ((= d ($ 0)) (cons (cdr (car tape)) (cons (car (car tape)) (cdr tape))))
                     ((= d ($ 1)) (cons (cons (car (cdr tape)) (car tape)) (cdr (cdr tape))))
                     ((= d ($ 2)) tape))))
                (turing-step
                 (lambda (tape state)
                   ($f
                    (displayTape (cons tape state))
                    (cond
                      ((and (= state s) (= (car (cdr tape)) n)) (turing-step (move-tape dir (cons (car tape) (cons n-prime (cdr (cdr tape))))) s-prime))...
                      (else (cons tape state))))))]
         (turing-step (start-tape tape-start) start-state))]))

(define-syntax (universal-turing-machine/log stx)
  (syntax-parse stx
    [(_)
     #'(letrec [
                (reverse
                 (lambda (lat rev)
                   (cond
                     ((null? lat) rev)
                     (default (reverse (cdr lat) (cons (car lat) rev))))))
                (blanks
                 (cons (& #\_) blanks))
                (start-tape
                 (lambda (starting)
                   (letrec [(helper
                             (lambda (lat)
                               (cond
                                 ((null? lat) blanks)
                                 (default (cons (car lat) (helper (cdr lat)))))))]
                     (cons blanks (helper starting)))))
                (move-tape
                 (lambda (d tape)
                   (cond
                     ((= d ($ 0)) (cons (cdr (car tape)) (cons (car (car tape)) (cdr tape))))
                     ((= d ($ 1)) (cons (cons (car (cdr tape)) (car tape)) (cdr (cdr tape))))
                     ((= d ($ 2)) tape))))
                (turing-step
                 (lambda (tape state transitions)
                   ($f
                    (displayTape (cons tape state))
                    (letrec [(parse-transitions
                              (lambda (t)
                                (cond
                                  ((null? t) (cons tape state))
                                  ((and (= state (car (car t))) (= (car (cdr tape)) (car (cdr (car t)))))
                                   (turing-step (move-tape (car (cdr (cdr (cdr (cdr (car t)))))) (cons (car tape) (cons  (car (cdr (cdr (cdr (car t))))) (cdr (cdr tape))))) (car (cdr (cdr (car t)))) transitions))
                                  (else (parse-transitions (cdr t))))))]
                      (parse-transitions transitions)))))]
                                                   
         (lambda (tape-start) (lambda (start-state) (lambda (transitions) (turing-step (start-tape tape-start) start-state transitions)))))]))