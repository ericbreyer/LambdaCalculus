;;;-------------------------------------About------------------------------------;;;
;;; Written by Eric Breyer, Mar 2022 - Apr 2023                                  ;;;
;;;                                                                              ;;;
;;; Using my "meta-language" in Racket that defines macros to convert more       ;;;
;;; conventional scheme syntax to pure lambda expressions in order to write      ;;;
;;; a universal turing machine and prove the equivalency of lambda calculas and  ;;;
;;; turing machines by simulation.                                               ;;;
;;;------------------------------------------------------------------------------;;;

;------------------------------------------------------------------;
;---The only things we can access are pure lambda calculus macros--;
;------------------------------------------------------------------;
#lang s-exp "./schemeSyntax.rkt"

;;; we dont have top level defines here so use a letrec to make a space to
;;; define variables/functions (with-define-do is just syntactic sugar for letrec)
;;; (with (define a b) ... do body ...) <-> (let-rec ((a b) ...) body ...)

(with

 ;--------------------------------;
 ;---Helper Functions/Variables---;
 ;--------------------------------;
 
 ; an infinite lazy stream of blanks for empty spaces on the tape
 [define blanks (cons/l (& #\_) blanks)] 

 ; a function to create the infinite double list for the tape,
 ; filled with starting state and blanks
 [define init-tape
   (lambda (starting-tape)
     (letrec {[helper
               (lambda (lat)
                 (cond
                   ((null? lat) blanks)
                   (default (cons/l (car lat) (helper (cdr lat))))))]}
       (cons blanks (helper starting-tape))))]

 ; a function to move the head across the tape,
 ; direction 0 is left, 1 is right, 2 is don't move
 [define move-tape
   (lambda (d tape)
     (let {[left-half  (car tape)]
           [right-half (cdr tape)]}
       (cond
         ((= d ($ 0)) (cons (cdr/l left-half) (cons/l (car/l left-half) right-half)))
         ((= d ($ 1)) (cons (cons/l (car/l right-half) left-half) (cdr/l right-half)))
         ((= d ($ 2)) tape))))]

 ;--------------------------;
 ;---Turing Machine Logic---;
 ;--------------------------;

 ; do one computation step (and call the next step (tail) recursively)
 ; takes the current tape, current head state, and the transition table
 [define turing-run
   (lambda (tape state transitions)
     ; make helper function to find a matching transition for the current state
     (letrec
         {[parse-transitions
           (lambda (t)
             (let* ([thisTransition (car t)]                   ; transition we are checking against
                    [s       (list-ref ($ 0) thisTransition)]  ; matching head state
                    [n       (list-ref ($ 1) thisTransition)]  ; mathching tape symbol
                    [s-prime (list-ref ($ 2) thisTransition)]  ; new head state
                    [n-prime (list-ref ($ 3) thisTransition)]  ; new tape symbol
                    [dir     (list-ref ($ 4) thisTransition)]) ; direction to move the head
               (cond
                 ((null? t) (cons tape state)) ; if there are no matching rules, halt and return the state
                 ((and (= state s) (= (car/l (cdr tape)) n)) ; if the current state and symbol match, do the transition
                  (turing-run (move-tape dir (cons (car tape) (cons/l n-prime (cdr/l (cdr tape))))) s-prime transitions))
                 (else (parse-transitions (cdr t))))))]} ; else try the next transition for a match
       (parse-transitions transitions)))] ; try each transition for a match in turn

 ; kick off the computation after initializing the double infinite tape
 [define universal-turing-machine
   (lambda (tape-start start-state transitions) (turing-run (init-tape tape-start) start-state transitions))]
 
 do

 ;------------------------;
 ;---Some Test Machines---;
 ;------------------------;
 
 ; palindrome checker, state 10 is reject, 11 is accept
 (displayTape (((universal-turing-machine
                 (list (& #\1) (& #\1) (& #\0) (& #\0) (& #\1) (& #\1)))
                ($ 0))
               (list 
                (T ($ 0) (& #\0) ($ 1) (& #\_)  ($ 1))
                (T ($ 1) (& #\0) ($ 1) (& #\0)  ($ 1))
                (T ($ 1) (& #\1) ($ 1) (& #\1)  ($ 1))
                (T ($ 0) (& #\1) ($ 2) (& #\_)  ($ 1))
                (T ($ 2) (& #\0) ($ 2) (& #\0)  ($ 1))
                (T ($ 2) (& #\1) ($ 2) (& #\1)  ($ 1))
                (T ($ 1) (& #\_) ($ 3) (& #\_)  ($ 0))
                (T ($ 3) (& #\0) ($ 5) (& #\_)  ($ 0))
                (T ($ 2) (& #\_) ($ 4) (& #\_)  ($ 0))
                (T ($ 4) (& #\1) ($ 5) (& #\_)  ($ 0))
                (T ($ 5) (& #\0) ($ 6) (& #\_)  ($ 0))
                (T ($ 6) (& #\0) ($ 6) (& #\0)  ($ 0))
                (T ($ 6) (& #\1) ($ 6) (& #\1)  ($ 0))
                (T ($ 5) (& #\1) ($ 7) (& #\_)  ($ 0))
                (T ($ 7) (& #\0) ($ 7) (& #\0)  ($ 0))
                (T ($ 7) (& #\1) ($ 7) (& #\1)  ($ 0))
                (T ($ 6) (& #\_) ($ 8) (& #\_)  ($ 1))
                (T ($ 8) (& #\0) ($ 0) (& #\_)  ($ 1))
                (T ($ 7) (& #\_) ($ 9) (& #\_)  ($ 1))
                (T ($ 9) (& #\1) ($ 0) (& #\_)  ($ 1))
                (T ($ 8) (& #\1) ($ 10) (& #\1) ($ 2))
                (T ($ 9) (& #\0) ($ 10) (& #\0) ($ 2))
                (T ($ 3) (& #\1) ($ 10) (& #\1) ($ 2))
                (T ($ 4) (& #\0) ($ 10) (& #\0) ($ 2))
                (T ($ 0) (& #\_) ($ 11) (& #\_) ($ 2))
                (T ($ 5) (& #\_) ($ 11) (& #\_) ($ 2))
                (T ($ 3) (& #\_) ($ 11) (& #\_) ($ 2))
                (T ($ 8) (& #\_) ($ 11) (& #\_) ($ 2))
                (T ($ 4) (& #\_) ($ 11) (& #\_) ($ 2))
                (T ($ 9) (& #\_) ($ 11) (& #\_) ($ 2))
                )))
 ; binary add
 (displayTape (((universal-turing-machine
                 (list (& #\1) (& #\1) (& #\0) (& #\_) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1)))
                ($ 0))
               (list
                ;move right to end of first number
                (T ($ 0) (& #\0) ($ 0) (& #\0) ($ 1))
                (T ($ 0) (& #\1) ($ 0) (& #\1) ($ 1))
                (T ($ 0) (& #\_) ($ 1) (& #\_) ($ 1))
                ;move right to end of second number
                (T ($ 1) (& #\0) ($ 1) (& #\0) ($ 1))
                (T ($ 1) (& #\1) ($ 1) (& #\1) ($ 1))
                (T ($ 1) (& #\_) ($ 2) (& #\_) ($ 0))
                ;subtract one from second number
                (T ($ 2) (& #\0) ($ 2) (& #\1) ($ 0))
                (T ($ 2) (& #\1) ($ 3) (& #\0) ($ 0))
                (T ($ 2) (& #\_) ($ 5) (& #\_) ($ 1))
                ;move left to end of first number
                (T ($ 3) (& #\0) ($ 3) (& #\0) ($ 0))
                (T ($ 3) (& #\1) ($ 3) (& #\1) ($ 0))
                (T ($ 3) (& #\_) ($ 4) (& #\_) ($ 0))
                ;add one to first number
                (T ($ 4) (& #\0) ($ 0) (& #\1) ($ 1))
                (T ($ 4) (& #\1) ($ 4) (& #\0) ($ 0))
                (T ($ 4) (& #\_) ($ 0) (& #\1) ($ 1))
                ;end - clean up
                (T ($ 5) (& #\0) ($ 5) (& #\_) ($ 1))
                (T ($ 5) (& #\1) ($ 5) (& #\_) ($ 1))
                (T ($ 5) (& #\_) ($ 6) (& #\_) ($ 2))
                ;end - move left to end of first number
                (T ($ 6) (& #\0) ($ 7) (& #\0) ($ 0))
                (T ($ 6) (& #\1) ($ 7) (& #\1) ($ 0))
                (T ($ 6) (& #\_) ($ 6) (& #\_) ($ 0))
                ;end - move left to start of first number
                (T ($ 7) (& #\0) ($ 7) (& #\0) ($ 0))
                (T ($ 7) (& #\1) ($ 7) (& #\1) ($ 0))
                (T ($ 7) (& #\_) ($ 8) (& #\_) ($ 1))
                ;halt
                )))
 ; fast(er) binary add
 (displayTape (universal-turing-machine
               (list (& #\1) (& #\1) (& #\0) (& #\+) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1))
               ($ 0)
               (list
                ;0 - move right to end of first number
                (T ($ 0) (& #\I) ($ 0) (& #\I) ($ 1))
                (T ($ 0) (& #\O) ($ 0) (& #\O) ($ 1))
                (T ($ 0) (& #\0) ($ 0) (& #\0) ($ 1))
                (T ($ 0) (& #\1) ($ 0) (& #\1) ($ 1))
                (T ($ 0) (& #\+) ($ 1) (& #\+) ($ 1))
                ;1 - move right to end of second number
                (T ($ 1) (& #\0) ($ 1) (& #\0) ($ 1))
                (T ($ 1) (& #\1) ($ 1) (& #\1) ($ 1))
                (T ($ 1) (& #\+) ($ 1) (& #\+) ($ 1))
                (T ($ 1) (& #\_) ($ 2) (& #\_) ($ 0))
                ;2 - take last digit from second number
                (T ($ 2) (& #\0) ($ 3) (& #\_) ($ 0))
                (T ($ 2) (& #\1) ($ 5) (& #\_) ($ 0))
                (T ($ 2) (& #\+) ($ 7) (& #\_) ($ 0))
                ; 3 - have 0 move left to end of first number
                (T ($ 3) (& #\0) ($ 3) (& #\0) ($ 0))
                (T ($ 3) (& #\1) ($ 3) (& #\1) ($ 0))
                (T ($ 3) (& #\+) ($ 4) (& #\+) ($ 0))
                ; 4 - add 0 to first number
                (T ($ 4) (& #\I) ($ 4) (& #\I) ($ 0))
                (T ($ 4) (& #\O) ($ 4) (& #\O) ($ 0))
                (T ($ 4) (& #\0) ($ 0) (& #\O) ($ 1))
                (T ($ 4) (& #\1) ($ 0) (& #\I) ($ 1))
                (T ($ 4) (& #\_) ($ 0) (& #\O) ($ 1))
                ; 5 - have 1 move left to end of first number
                (T ($ 5) (& #\0) ($ 5) (& #\0) ($ 0))
                (T ($ 5) (& #\1) ($ 5) (& #\1) ($ 0))
                (T ($ 5) (& #\+) ($ 6) (& #\+) ($ 0))
                ; 6 - add 1 to first number
                (T ($ 6) (& #\I) ($ 6) (& #\I) ($ 0))
                (T ($ 6) (& #\O) ($ 6) (& #\O) ($ 0))
                (T ($ 6) (& #\0) ($ 0) (& #\I) ($ 1))
                (T ($ 6) (& #\1) ($ 8) (& #\O) ($ 0))
                (T ($ 6) (& #\_) ($ 0) (& #\I) ($ 1))
                ; 8 - carry 1 to first number
                (T ($ 8) (& #\0) ($ 0) (& #\1) ($ 1))
                (T ($ 8) (& #\1) ($ 8) (& #\0) ($ 0))
                (T ($ 8) (& #\_) ($ 0) (& #\1) ($ 1))
                ; 7 - end - clean up
                (T ($ 7) (& #\I) ($ 7) (& #\1) ($ 0))
                (T ($ 7) (& #\O) ($ 7) (& #\0) ($ 0))
                (T ($ 7) (& #\0) ($ 7) (& #\0) ($ 0))
                (T ($ 7) (& #\1) ($ 7) (& #\1) ($ 0))
                (T ($ 7) (& #\_) ($ 9) (& #\_) ($ 1))
                ;halt
                ))))