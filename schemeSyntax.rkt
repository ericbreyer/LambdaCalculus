#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse racket/syntax racket/format))
(require (for-meta 2 syntax/parse racket/syntax racket/base))

;;;-------About-------
;;; Written by Eric Breyer, Mar 2022
;;;
;;; Writing a "meta-language" in Racket that
;;; defines macros to convert more conventional scheme
;;; syntax to pure lambda expressions. All computations
;;; should be done with pure lambda calculus
;;; ------------------

;;;https://www.greghendershott.com/fear-of-macros/all.html#%28part._begin-for-syntax%29

;export fundemental racket builtin functions
(provide lambda
         λ
         #%module-begin
         #%app
         #%datum
         #%top-interaction)

;;;---Functions to convert lambda expressions back to scheme data for console output---
(provide (rename-out [displaylnConvert displayln]) displayHash)
(define displaylnConvert
  (lambda (type data)
    (displayln ((hash-ref displayHash type) data))))

(define convertChurchBool
  (lambda (bool)
    ((bool #t) #f)))

(define convertChurchNumeral
  (lambda(churchNum)
    ((churchNum (lambda(x) (add1 x))) 0)))

(define convertChurchPair
  (lambda (innerConversionFunc)
    (lambda (p)
      (cons (innerConversionFunc (FIRST p)) (innerConversionFunc (SECOND p))))))

(define convertChurchList
  (lambda (innerConversionFunc)
    (lambda (lat)
      (cond
        ((convertChurchBool (NIL? lat)) '())
        (#t (cons (innerConversionFunc (FIRST lat))
                  ((convertChurchList innerConversionFunc) (SECOND lat))))))))

(define convertSignedChurchNumeral
  (lambda(churchNum)
    (- (convertChurchNumeral (FIRST churchNum)) (convertChurchNumeral (SECOND churchNum)))))

(define displayHash (hash
                     "bool" convertChurchBool
                     "natural" convertChurchNumeral
                     "natural pair" (convertChurchPair convertChurchNumeral)
                     "natural list" (convertChurchList convertChurchNumeral)
                     "natural list list" (convertChurchList (convertChurchList convertChurchNumeral))
                     "signed" convertSignedChurchNumeral
                     "signed list" (convertChurchList convertSignedChurchNumeral)))

;;;***-------------------------------------***
;;;***-----"defining scheme in scheme"-----***
;;;***-------------------------------------***

;;;------------------------
;;;---internal utilities---
;;;------------------------

;macros to convert syntax and datum to strings (mainly for logging purposes)
(begin-for-syntax
  (define-syntax (datum->str stx)
    (syntax-parse stx
      #:literals (~v)
      [(_ datum) #`(substring (~v datum) 1)]))
  (define-syntax (syntax->str stx)
    (syntax-parse stx
      #:literals (~v)
      [(_ syntax) #`(substring (~v (syntax->datum syntax)) 1)])))

;define syntax class for a valid lc expression---
;should make all other macros' error checking more robust and simple
(begin-for-syntax

  (define-literal-set churchificationOperations
    #:datum-literals ($ % toChurch) ())

  (define-syntax-class toBeChurchy
    #:description "racket literal churchification operation"
    #:literal-sets (churchificationOperations)
    (pattern (op expr)
             #:fail-unless ((literal-set->predicate churchificationOperations) #'op)
             (string-append "\"" (syntax->str #'op) "\" is not a churchification operation")))

  (define-syntax-class expr/lc
    #:description "lambda expression - one of:
                   identifier                - :id
                   abstraction               - (lambda (:id ...) :lambda-expression ...+)
                   application               - (:id :lambda-expression)
                   racket literal conversion - (:churchification-opperation :racket-literal)"
    #:literals (lambda)
    (pattern (~or* (lambda (:id ...) :expr/lc ...+)
                   (:expr/lc :expr/lc ...)
                   :id
                   :toBeChurchy)))
  )

;defining a simple macro - a macro which is just a simple replace for a lambda calculus expression
(define-syntax (define-simple-func stx)
  (syntax-parse stx
    [(_ name:id alt:id func:expr/lc argType 0)
     #'(begin
         (provide (rename-out [name alt]))
         (define-syntax (name stx)
           (define-syntax-class alt-names
             #:description "set of alternate names"
             #:datum-literals (name alt)
             (pattern (~or name alt)))

           (syntax-parse stx
             #:literals (lambda)
             [:alt-names #'func])))]

    [(_ name:id alt:id func:expr/lc argType 1)
     #'(begin
         (provide (rename-out [name alt]))
         (define-syntax (name stx)
           (define-syntax-class alt-names
             #:description "set of alternate names"
             #:datum-literals (name alt)
             (pattern (~or name alt)))

           (syntax-parse stx
             #:literals (lambda)
             [(_ arg) #:declare arg argType #'(func arg)]
             [:alt-names #'func])))]
    [(_ name:id alt:id func:expr/lc argType 2)
     #'(begin
         (provide (rename-out [name alt]))
         (define-syntax (name stx)
           (define-syntax-class alt-names
             #:description "set of alternate names"
             #:datum-literals (name alt)
             (pattern (~or name alt)))

           (syntax-parse stx
             #:literals (lambda)
             [(_ arg1 arg2) #:declare arg1 argType #:declare arg2 argType #'((func arg1) arg2)]
             [(_ arg) #:declare arg argType #'(func arg)]
             [a:alt-names #'func])))]
    [(_ name:id alt:id func:expr/lc argType 3)
     #'(begin
         (provide (rename-out [name alt]))
         (define-syntax (name stx)
           (define-syntax-class alt-names
             #:description "set of alternate names"
             #:datum-literals (name alt)
             (pattern (~or name alt)))

           (syntax-parse stx
             #:literals (lambda)
             [(_ arg1 arg2 arg3) #:declare arg1 argType
                                 #:declare arg2 argType
                                 #:declare arg3 argType #'(((func arg1) arg2) arg3)]
             [(_ arg1 arg2) #:declare arg1 argType #:declare arg2 argType #'((func arg1) arg2)]
             [(_ arg) #:declare arg argType #'(func arg)]
             [a:alt-names #'func])))]))


;;;---------------------
;;;---Let expressions---
;;;---------------------

(define-syntax (let/lc stx)

  (define-syntax-class bp
    #:description "binding pair"
    (pattern (x:id y:expr/lc)))

  (syntax-parse stx
    #:literals (lambda)
    [(let/lc (binding:bp ...) body:expr/lc ...+)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(binding.x ...)))
     "duplicate variable name"
     #'(curry/args ((lambda (binding.x ...) body ...) binding.y ...))]))

(define-syntax (curry/args stx)
  (syntax-parse stx
    #:literals (lambda)
    [(curry/args/lc ((lambda (a:id) body:expr/lc ...) c:expr/lc)) #'((lambda (a) body ...)c)]
    [(curry/args/lc ((lambda (a:id b:id ...) body:expr/lc ...) c:expr/lc d:expr/lc ...))
     #'((lambda (a) (curry/args/lc ((lambda (b ...) body ...) d ...))) c)]))

;;;------------------------------------------------------------
;;;---Scheme numbers to church definition of natural numbers---
;;;------------------------------------------------------------

(provide (rename-out [toChurch $]))
(define-syntax (toChurch stx)
  (syntax-parse stx
    #:literals (lambda)
    [(_ num:nat) #'(toChurch num (lambda (f) (lambda (x) x)))]                      ; entry case
    [(_ 0 (lambda (f) (lambda (x) body))) #'(lambda (f) (lambda (x) body))]         ; base case
    [(_ appsLeft:nat (lambda (f) (lambda (x) body)))                                ; recursive case
     #:with newAppsLeft (datum->syntax #'appsLeft (- (syntax->datum #'appsLeft) 1))
     #'(toChurch newAppsLeft (lambda (f) (lambda (x) (f body))))]))

;;;---------------
;;;---Aritmetic---
;;;---------------

;define syntax class for a valid church numeral

(define-simple-func SUCC add1 (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) expr/lc 1)
(define-simple-func ADD  +    (lambda (m) (lambda (n) ((m SUCC) n)))               expr/lc 2)
(define-simple-func MULT *    (lambda (m) (lambda (n) ((m (ADD n)) (toChurch 0)))) expr/lc 2)
(define-simple-func EXPT expt (lambda (b) (lambda (e) (e b)))                      expr/lc 2)

;;;------------------------------------------------------
;;;---Church Booleans, Boolean algebra, and predicates---
;;;------------------------------------------------------

(define-simple-func TRUE  $t    (lambda (a) (lambda (b) a))               expr/lc 2)
(define-simple-func FALSE $f    (lambda (a) (lambda (b) b))               expr/lc 2)
(define-simple-func NOT   not   (lambda (p) ((p FALSE) TRUE))             expr/lc 1)
(define-simple-func AND   and   (lambda (p) (lambda (q) ((p q) p)))       expr/lc 2)
(define-simple-func OR    or    (lambda (p) (lambda (q) ((p p) q)))       expr/lc 2)
(define-simple-func ZERO? zero? (lambda (n) ((n (lambda(x) FALSE)) TRUE)) expr/lc 2)

;;;------------------------------
;;;---Pairs and pair functions---
;;;------------------------------

(define-simple-func PAIR   cons  (lambda (x) (lambda (y) (lambda (f) ((f x) y)))) expr/lc 3)
(define-simple-func FIRST  car   (lambda (p) (p TRUE))                            expr/lc 1)
(define-simple-func SECOND cdr   (lambda (p) (p FALSE))                           expr/lc 1)
(define-simple-func NIL    null  (lambda (x) TRUE)                                expr/lc 0)
(define-simple-func NIL?   null? (lambda (p) (p (lambda (x) (lambda (y) FALSE)))) expr/lc 1)

;;;----------------------
;;;---Nice List Syntax---
;;;----------------------

(provide (rename-out (LIST list)))
(define-syntax (LIST stx)
  (syntax-parse stx
    [(_ () pairs) #'pairs]                                                ;base case
    [(_ (arg ... last) pairs) #'(LIST (arg ...) ((PAIR last) pairs))]     ;recursive case
    [(_ arg:expr/lc ... last:expr/lc) #'(LIST (arg ...) (PAIR last NIL))] ;entry case
    [(_ ()) #'NIL]                                                        ;special case
    [(_ ) #'NIL]))                                                        ;special case

;;;----------------------------
;;;---Subtraction arithmetic---
;;;----------------------------

(define-simple-func Φ    phi (lambda (p) ((PAIR (SECOND p)) (SUCC (SECOND p)))) expr/lc 1)
(define-simple-func PRED sub1 (lambda (n) (FIRST ((n Φ) ((PAIR (toChurch 0)) (toChurch 0))))) expr/lc 1)
(define-simple-func SUB - (lambda (m) (lambda (n) ((n PRED) m))) expr/lc 2)
(define-simple-func DIV / (rec-def div
                                   (lambda (x)
                                     (lambda (y)
                                       (mycond
                                        ((ZERO? y) (toChurch 0))
                                        ((ZERO? (SUB y x)) (SUCC ((div (SUB x y)) y)))
                                        (default (toChurch 0)))))) expr/lc 2)


(define-simple-func GEQ? >= (lambda (arg1) (lambda (arg2) (ZERO? (SUB arg2 arg1)))) expr/lc 2)
(define-simple-func EQ? = (lambda (arg1) (lambda (arg2)  (AND (GEQ? arg1 arg2) (GEQ? arg2 arg1)))) expr/lc 2)

;;;--------------------------
;;;---Recursion and letrec---
;;;--------------------------

(define-simple-func M m (lambda (f) (f f)) expr/lc 1)

;if only we used lazy scheme
;(define-syntax Y-lazy
;    (syntax-rules ()
;      [(_ func) ((lambda (f) (M (lambda (x) (f (x x))))) func)]))

(provide (rename-out (Y-app Y)))
(define-syntax (Y-app stx)
  (syntax-parse stx
    #:literals (lambda)
    #:datum-literals (Y-app)
    [(Y-app (lambda (funcName) (lambda (z ...) body))) ; case for multiple argument functions
     #'((lambda (f) (M (lambda (x) (f (lambda (z ...) ((x x) z ...))))))
        (lambda (funcName) (lambda (z ...) body)))]
    [(Y-app func) #'((lambda (f) (M (lambda (x) (f (lambda (z) ((x x) z)))))) func)])) ; "traditional" case (1 arg)

(provide (rename-out (rec-def rec)))
(define-syntax (rec-def stx)
  (syntax-parse stx
    #:literals (lambda)
    #:datum-literals (Y-app)
    [(_ funcName:id body:expr/lc) #'(Y-app (lambda (funcName) body))]))

(provide (rename-out (let-rec letrec)))
(define-syntax let-rec
  (syntax-rules ()
    [(_ ((a b) ...) body ...) (curry-rec ((lambda (a ...) body ...) b ...))]))

(define-syntax curry-rec
  (syntax-rules ()
    [(_ ((lambda (a) body ...) c)) ((lambda (a) body ...) (rec-def a c))]
    [(_ ((lambda (a b ...) body ...) c d ...))
     ((lambda (a) (curry-rec ((lambda (b ...) body ...) d ...))) (rec-def a c))]))
;              ^                                                 ^          
;              "external" let definition                         "internal" recursive definition

;;;------------------
;;;---Conditionals---
;;;------------------

;only eval the executed branch by delaying with lambdas and only running the picked one

(provide if)
(define-syntax if
  (syntax-rules ()
    [(_ pred then else) (((pred (lambda () then)) (lambda () else)))]
    [(_ pred then) (((pred (lambda () then)) (lambda () FALSE)))]))

; cond is essentially a chain of ifs
(provide (rename-out (mycond cond)))
(define-syntax mycond
  (syntax-rules ()
    ((mycond body ()) (body)) ; base case - execute the chosen body
    ((mycond built ((pred body) ... (lPred lBody))) ; recursive case - lazy evaluation with lambdas
     (mycond ((lPred (lambda () lBody)) built) ((pred body) ...)))
    ((mycond (pred body) ... (lPred lBody)) (mycond ((lPred (lambda () lBody)) (lambda () FALSE)) ((pred body) ...))))) ; entry case - cond syntax

(provide (rename-out (TRUE default)))
(provide (rename-out (TRUE else)))

;(define-syntax default
;  (syntax-rules ()
;    ((default stuff f) stuff)))

;;;--------------------
;;;---Signed Numbers---
;;;--------------------

(define-syntax (CONVERTs stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (x) (PAIR x ($ 0)))arg)]
    [_ #'(lambda (x) (PAIR x ($ 0)))]))

(define-syntax (NEGs stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (x) ((PAIR (SECOND x)) (FIRST x)))arg)]
    [_ #'(lambda (x) ((PAIR (SECOND x)) (FIRST x)))]))


(define-syntax define-syntax/alias
  (syntax-rules ()
    [(define-syntax/alias (names ...) stx mycase) (define-syntaxes (names ...)
                                                    (let [(rule
                                                           (lambda (stx) mycase))]
                                                      (values (begin 'names rule) ...)))]))

(define-syntax/alias (toChurchs %) stx
  (syntax-case stx ()
    [(_ num)
     (with-syntax ([newstx
                    (with-syntax [(n (datum->syntax #'num (abs (syntax->datum #'num))))]
                      (cond
                        ((not number?) (error "not signed church numeralable"))
                        ((< (syntax->datum #'num) 0) #'(NEGs (CONVERTs ($ n))))
                        (#t #'(CONVERTs ($ n)))))])
       #'newstx)]))

(define-syntax (SIMPLIFYs stx)
  (syntax-case stx ()
    [(_ num) #'((rec-def simp
                         (lambda (x)
                           (cond
                             ((OR (ZERO? (FIRST x)) (ZERO? (SECOND x))) x)
                             (TRUE (simp (PAIR (PRED (FIRST x)) (PRED (SECOND x)))))))) num)]))

(define-syntax/alias (ADDs +s) stx
  (syntax-case stx ()
    [(_ m n) #'(((lambda (x) (lambda (y) (SIMPLIFYs ((PAIR ((ADD (FIRST x)) (FIRST y)))
                                                     ((ADD (SECOND x)) (SECOND y))))))
                 m)n)]
    [(_ m) #'((lambda (x) (lambda (y) (SIMPLIFYs ((PAIR ((ADD (FIRST x)) (FIRST y)))
                                                  ((ADD (SECOND x)) (SECOND y))))))
              m)]
    [_ #'(lambda (x) (lambda (y) (SIMPLIFYs ((PAIR ((ADD (FIRST x)) (FIRST y)))
                                             ((ADD (SECOND x)) (SECOND y))))))]))

(define-syntax/alias (SUBs -s) stx
  (syntax-case stx ()
    [(_ m n) #'(((lambda (x) (lambda (y) (SIMPLIFYs ((PAIR ((ADD (FIRST x)) (SECOND y)))
                                                     ((ADD (SECOND x)) (FIRST y))))))
                 m)n)]
    [(_ m) #'((lambda (x) (lambda (y) (SIMPLIFYs ((PAIR ((ADD (FIRST x)) (SECOND y)))
                                                  ((ADD (SECOND x)) (FIRST y))))))
              m)]
    [_ #'(lambda (x) (lambda (y) (SIMPLIFYs ((PAIR ((ADD (FIRST x)) (SECOND y)))
                                             ((ADD (SECOND x)) (FIRST y))))))]))

(define-syntax/alias (MULTs *s) stx
  (syntax-case stx ()
    [(_ m n) #'(((lambda (x) (lambda (y) (SIMPLIFYs (PAIR [ADD (MULT (FIRST x) (FIRST y))
                                                               (MULT (SECOND x) (SECOND y))]
                                                          [ADD (MULT (FIRST x) (SECOND y))
                                                               (MULT (SECOND x) (FIRST y))]))))
                 m)n)]
    [(_ m) #'((lambda (x) (lambda (y) (SIMPLIFYs (PAIR [ADD (MULT (FIRST x) (FIRST y))
                                                            (MULT (SECOND x) (SECOND y))]
                                                       [ADD (MULT (FIRST x) (SECOND y))
                                                            (MULT (SECOND x) (FIRST y))]))))
              m)]
    [_ #'(lambda (x) (lambda (y) (SIMPLIFYs (PAIR [ADD (MULT (FIRST x) (FIRST y))
                                                       (MULT (SECOND x) (SECOND y))]
                                                  [ADD (MULT (FIRST x) (SECOND y))
                                                       (MULT (SECOND x) (FIRST y))]))))]))

(define-syntax/alias (DIVs /s) stx
  (syntax-case stx ()
    [(_ m n) #'(((lambda (x) (lambda (y) (SIMPLIFYs (PAIR [ADD (DIV (FIRST x) (FIRST y))
                                                               (DIV (SECOND x) (SECOND y))]
                                                          [ADD (DIV (FIRST x) (SECOND y))
                                                               (DIV (SECOND x) (FIRST y))]))))
                 m)n)]
    [(_ m) #'((lambda (x) (lambda (y) (SIMPLIFYs (PAIR [ADD (DIV (FIRST x) (FIRST y))
                                                            (DIV (SECOND x) (SECOND y))]
                                                       [ADD (DIV (FIRST x) (SECOND y))
                                                            (DIV (SECOND x) (FIRST y))]))))
              m)]
    [_ #'(lambda (x) (lambda (y) (SIMPLIFYs (PAIR [ADD (DIV (FIRST x) (FIRST y))
                                                       (DIV (SECOND x) (SECOND y))]
                                                  [ADD (DIV (FIRST x) (SECOND y))
                                                       (DIV (SECOND x) (FIRST y))]))))]))

(define-syntax ZERO?s
  (syntax-rules ()
    [(_ num) ((lambda (n) (AND (ZERO? (FIRST n)) (ZERO? (SECOND n)))) num)]))

;;;---------------
;;;---rationals---
;;;---------------

(define-syntax (CONVERTr stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (x) (PAIR x (% 0)))arg)]
    [_ #'(lambda (x) (PAIR x (% 0)))]))

(define-syntax (INVERTs stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (x) ((PAIR (SECOND x)) (FIRST x)))arg)]
    [_ #'(lambda (x) ((PAIR (SECOND x)) (FIRST x)))]))

;;;-------------
;;;---vectors---
;;;-------------

(provide vec-lit)
(define-syntax (vec-lit stx)
  (syntax-parse stx
    #:literals (lambda)
    ;the recursive chaining is for currying
    ((vec-lit (lambda (f) built:expr) (arg rest ...)) #'(vec-lit (lambda (f) (built arg)) (rest ...)))
    ((vec-lit (lambda (f) built:expr) ()) #'(lambda (f) built))
    ((vec-lit arg:expr/lc rest:expr/lc ...) #'(vec-lit (lambda (f) (f arg)) (rest ...)))))

(define-syntax (vec-ref-selector stx)
  (syntax-parse stx
    #:literals (lambda)
    ((vec-ref-selector 0 expr)
     #'expr)
    ((vec-ref-selector num (lambda (lchar) body))
     #:with char (datum->syntax #'num (string->symbol (substring (~v (integer->char (+ 64 (syntax->datum #'num)))) 2)))
     #:with newnum (datum->syntax #'num (- (syntax->datum #'num) 1))
     #'(vec-ref-selector newnum (lambda (char) (lambda (lchar) body))))
    ((vec-ref-selector num sel)
     #:with char (datum->syntax #'num (string->symbol (substring (~v (integer->char (+ 64 (syntax->datum #'num)))) 2)))
     #:with selchar (datum->syntax #'sel (string->symbol (substring (~v (integer->char (+ 65 (syntax->datum #'sel)))) 2)))
     #:with newnum (datum->syntax #'num (- (syntax->datum #'num) 1))
     #'(vec-ref-selector newnum (lambda (char) selchar)))))


(provide vec-ref)
(define-syntax (vec-ref stx)
  (syntax-parse stx
    #:literals (lambda)
    ((vec-ref len vec[idx]) #'(vec (vec-ref-selector len idx)))
    ((vec-ref vec idx len) #'(vec (vec-ref-selector len idx)))))


(define-syntax (vec-set-selector stx)
  (syntax-parse stx
    #:literals (lambda)
    ((vec-set-selector 0 expr)
     #'expr)
    ((vec-set-selector num (lambda (z) (lambda (chars ...+) (vec-lit body ...))))
     #:with char (datum->syntax #'num (string->symbol (substring (~v (integer->char (+ 64 (syntax->datum #'num)))) 2)))
     #:with newnum (datum->syntax #'num (- (syntax->datum #'num) 1))
     #'(vec-set-selector newnum (lambda (z) (lambda (char chars ...) (vec-lit char body ... )))))
    ((vec-set-selector num sel (lambda (chars ...+) (vec-lit body ...)))
     #:when (= (syntax->datum #'num) (+ 1 (syntax->datum #'sel)))
     #:with char (datum->syntax #'num (string->symbol (substring (~v (integer->char (+ 64 (syntax->datum #'num)))) 2)))
     #:with newnum (datum->syntax #'num (- (syntax->datum #'num) 1))
     #'(vec-set-selector newnum (lambda (z) (lambda (char chars ...) (vec-lit z body ... )))))
    ((vec-set-selector num sel (lambda (chars ...+) (vec-lit body ...)))
     #:with char (datum->syntax #'num (string->symbol (substring (~v (integer->char (+ 64 (syntax->datum #'num)))) 2)))
     #:with newnum (datum->syntax #'num (- (syntax->datum #'num) 1))
     #'(vec-set-selector newnum sel (lambda (char chars ...) (vec-lit char body ... ))))
    ((vec-set-selector num sel)
     #:when (= (syntax->datum #'num) (+ 1 (syntax->datum #'sel)))
     #:with char (datum->syntax #'num (string->symbol (substring (~v (integer->char (+ 64 (syntax->datum #'num)))) 2)))
     #:with newnum (datum->syntax #'num (- (syntax->datum #'num) 1))
     #'(vec-set-selector newnum (lambda (z) (lambda (char) (vec-lit z)))))
    ((vec-set-selector num sel)
     #:with char (datum->syntax #'num (string->symbol (substring (~v (integer->char (+ 64 (syntax->datum #'num)))) 2)))
     #:with newnum (datum->syntax #'num (- (syntax->datum #'num) 1))
     #'(vec-set-selector newnum sel (lambda (char) (vec-lit char))))
    ))

(provide vec-set)
(define-syntax (vec-set stx)
  (syntax-parse stx
    #:literals (lambda)
    ((vec-set vec set:expr/lc idx len) #'(vec (curry ((vec-set-selector len idx) set))))))

(provide time)
;(define-syntax (lambda stx)
;  (syntax-parse stx
;    ((lambda (args ...) body ...) #'(λ (args ...) (sleep .1) body ...))))