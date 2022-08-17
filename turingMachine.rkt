#lang s-exp "./schemeSyntax.rkt"
(require "./turingmachinemacro.rkt")
;(displayTape
#;(turing-machine
 (list (& #\1) (& #\0) (& #\0) (& #\0) (& #\0) (& #\1) (& #\_))
 ($ 0)
 (($ 0) (& #\0) ($ 1) (& #\_)  ($ 1))
 (($ 1) (& #\0) ($ 1) (& #\0)  ($ 1))
 (($ 1) (& #\1) ($ 1) (& #\1)  ($ 1))
 (($ 0) (& #\1) ($ 2) (& #\_)  ($ 1))
 (($ 2) (& #\0) ($ 2) (& #\0)  ($ 1))
 (($ 2) (& #\1) ($ 2) (& #\1)  ($ 1))
 (($ 1) (& #\_) ($ 3) (& #\_)  ($ 0))
 (($ 3) (& #\0) ($ 5) (& #\_)  ($ 0))
 (($ 2) (& #\_) ($ 4) (& #\_)  ($ 0))
 (($ 4) (& #\1) ($ 5) (& #\_)  ($ 0))
 (($ 5) (& #\0) ($ 6) (& #\_)  ($ 0))
 (($ 6) (& #\0) ($ 6) (& #\0)  ($ 0))
 (($ 6) (& #\1) ($ 6) (& #\1)  ($ 0))
 (($ 5) (& #\1) ($ 7) (& #\_)  ($ 0))
 (($ 7) (& #\0) ($ 7) (& #\0)  ($ 0))
 (($ 7) (& #\1) ($ 7) (& #\1)  ($ 0))
 (($ 6) (& #\_) ($ 8) (& #\_)  ($ 1))
 (($ 8) (& #\0) ($ 0) (& #\_)  ($ 1))
 (($ 7) (& #\_) ($ 9) (& #\_)  ($ 1))
 (($ 9) (& #\1) ($ 0) (& #\_)  ($ 1))
 (($ 8) (& #\1) ($ 10) (& #\1)  ($ 2))
 (($ 9) (& #\0) ($ 10) (& #\0)  ($ 2))
 (($ 3) (& #\1) ($ 10) (& #\1)  ($ 2))
 (($ 4) (& #\0) ($ 10) (& #\0)  ($ 2))
 (($ 0) (& #\_) ($ 11) (& #\_)  ($ 2))
 (($ 5) (& #\_) ($ 11) (& #\_)  ($ 2))
 (($ 3) (& #\_) ($ 11) (& #\_)  ($ 2))
 (($ 8) (& #\_) ($ 11) (& #\_)  ($ 2))
 (($ 4) (& #\_) ($ 11) (& #\_)  ($ 2))
 (($ 9) (& #\_) ($ 11) (& #\_)  ($ 2))
 );)

;(displayTape
#;(turing-machine
 (list (& #\1) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1) (& #\1) (& #\_) (& #\1) (& #\1))
 ($ 0)

 ;move right to end of first number
 (($ 0) (& #\0) ($ 0) (& #\0)  ($ 1))
 (($ 0) (& #\1) ($ 0) (& #\1)  ($ 1))
 (($ 0) (& #\_) ($ 1) (& #\_)  ($ 1))
 ;move right to end of second number
 (($ 1) (& #\0) ($ 1) (& #\0)  ($ 1))
 (($ 1) (& #\1) ($ 1) (& #\1)  ($ 1))
 (($ 1) (& #\_) ($ 2) (& #\_)  ($ 0))
 ;subtract one from second number
 (($ 2) (& #\0) ($ 2) (& #\1)  ($ 0))
 (($ 2) (& #\1) ($ 3) (& #\0)  ($ 0))
 (($ 2) (& #\_) ($ 5) (& #\_)  ($ 1))
 ;move left to end of first number
 (($ 3) (& #\0) ($ 3) (& #\0)  ($ 0))
 (($ 3) (& #\1) ($ 3) (& #\1)  ($ 0))
 (($ 3) (& #\_) ($ 4) (& #\_)  ($ 0))
 ;add one to first number
 (($ 4) (& #\0) ($ 0) (& #\1)  ($ 1))
 (($ 4) (& #\1) ($ 4) (& #\0)  ($ 0))
 (($ 4) (& #\_) ($ 0) (& #\1)  ($ 1))
 ;end - clean up
 (($ 5) (& #\0) ($ 5) (& #\_)  ($ 1))
 (($ 5) (& #\1) ($ 5) (& #\_)  ($ 1))
 (($ 5) (& #\_) ($ 6) (& #\_)  ($ 2))
 ;end - move left to end of first number
 (($ 6) (& #\0) ($ 7) (& #\0)  ($ 0))
 (($ 6) (& #\1) ($ 7) (& #\1)  ($ 0))
 (($ 6) (& #\_) ($ 6) (& #\_)  ($ 0))
 ;end - move left to start of first number
 (($ 7) (& #\0) ($ 7) (& #\0)  ($ 0))
 (($ 7) (& #\1) ($ 7) (& #\1)  ($ 0))
 (($ 7) (& #\_) ($ 8) (& #\_)  ($ 1))
 ;halt
 );)
            
(displayTape ((((universal-turing-machine)
 (list (& #\1) (& #\1) (& #\_) (& #\1) (& #\1) (& #\1) (& #\_) (& #\_) (& #\_) (& #\_) (& #\1) (& #\1)))
 ($ 0))
 (list
 ;move right to end of first number
 (L ($ 0) (& #\0) ($ 0) (& #\0)  ($ 1))
 (L ($ 0) (& #\1) ($ 0) (& #\1)  ($ 1))
 (L ($ 0) (& #\_) ($ 1) (& #\_)  ($ 1))
 ;move right to end of second number
 (L ($ 1) (& #\0) ($ 1) (& #\0)  ($ 1))
 (L ($ 1) (& #\1) ($ 1) (& #\1)  ($ 1))
 (L ($ 1) (& #\_) ($ 2) (& #\_)  ($ 0))
 ;subtract one from second number
 (L ($ 2) (& #\0) ($ 2) (& #\1)  ($ 0))
 (L ($ 2) (& #\1) ($ 3) (& #\0)  ($ 0))
 (L ($ 2) (& #\_) ($ 5) (& #\_)  ($ 1))
 ;move left to end of first number
 (L ($ 3) (& #\0) ($ 3) (& #\0)  ($ 0))
 (L ($ 3) (& #\1) ($ 3) (& #\1)  ($ 0))
 (L ($ 3) (& #\_) ($ 4) (& #\_)  ($ 0))
 ;add one to first number
 (L ($ 4) (& #\0) ($ 0) (& #\1)  ($ 1))
 (L ($ 4) (& #\1) ($ 4) (& #\0)  ($ 0))
 (L ($ 4) (& #\_) ($ 0) (& #\1)  ($ 1))
 ;end - clean up
 (L ($ 5) (& #\0) ($ 5) (& #\_)  ($ 1))
 (L ($ 5) (& #\1) ($ 5) (& #\_)  ($ 1))
 (L ($ 5) (& #\_) ($ 6) (& #\_)  ($ 2))
 ;end - move left to end of first number
 (L ($ 6) (& #\0) ($ 7) (& #\0)  ($ 0))
 (L ($ 6) (& #\1) ($ 7) (& #\1)  ($ 0))
 (L ($ 6) (& #\_) ($ 6) (& #\_)  ($ 0))
 ;end - move left to start of first number
 (L ($ 7) (& #\0) ($ 7) (& #\0)  ($ 0))
 (L ($ 7) (& #\1) ($ 7) (& #\1)  ($ 0))
 (L ($ 7) (& #\_) ($ 8) (& #\_)  ($ 1))
 ;halt
 )))
            