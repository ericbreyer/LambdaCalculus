;;;-------------------------------------About------------------------------------;;;
;;; Written by Eric Breyer, Mar 2022 - Apr 2023                                  ;;;
;;;                                                                              ;;;
;;; Writing a "meta-language" in Racket that defines macros to convert more      ;;;
;;; conventional scheme syntax to pure lambda expressions. All computation is    ;;;
;;; done with pure lambda calculus                                               ;;;
;;;------------------------------------------------------------------------------;;;

;;;====================================Preamble==================================;;;
;;; utility functions and output functions                                       ;;;
;;;==============================================================================;;;

;--------------------------------------------;
;---Imports for various compilation phases---;
;--------------------------------------------;
#lang racket
(require syntax/parse (for-syntax syntax/parse) (for-meta 2 syntax/parse))
(require (for-syntax racket/syntax) (for-meta 2 racket/syntax))
(require (for-syntax racket/base) (for-meta 2 racket/base))
(require (for-syntax racket/format))

;--------------------------------------------------;
;---export fundemental racket builtins functions---;
;--------------------------------------------------;

(provide #%module-begin #%datum #%top-interaction)

;------------------------------------------------------------------------------------;
;---Functions to convert lambda expressions back to scheme data for console output---;
;------------------------------------------------------------------------------------;

(define convertChurchBool
  (lambda (bool)
    ((bool #t) #f)))

(define convertChurchNumeral
  (lambda(churchNum)
    ((churchNum (lambda(x) (add1 x))) 0)))

(define convertChurchChar
  (lambda(churchNum)
    (let [(char (convertChurchNumeral churchNum))]
      (cond
        ((or (< char 30) (> char 100)) (raise "not a valid character"))
        (#t (car (cons (integer->char char) char)))))))

(define convertChurchPair
  (lambda (innerConversionFunc)
    (lambda (p)
      (cons (innerConversionFunc (FIRST p)) (innerConversionFunc (SECOND p))))))

(define convertChurchList
  (lambda (innerConversionFunc)
    (lambda (lat)
      (cond
        ((not (boolean? (convertChurchBool (NIL? lat)))) (raise "brah"))
        ((convertChurchBool (NIL? lat)) '())
        (#t (cons (innerConversionFunc (FIRST lat))
                  ((convertChurchList innerConversionFunc) (SECOND lat))))))))

(define convertSignedChurchNumeral
  (lambda(churchNum)
    (- (convertChurchNumeral (FIRST churchNum))
       (convertChurchNumeral (SECOND churchNum)))))

(define convertRationalChurchNumeral
  (lambda(churchNum)
    (/ (convertSignedChurchNumeral (FIRST churchNum))
       (convertSignedChurchNumeral (SECOND churchNum)))))

(define convertTuringTape ; for my turing machines
  (lambda (tape)
    (letrec [(helper
              (lambda (lats idx half)
                (cond
                  ((and (convertChurchBool (EQ? idx (toChurch 0))) (equal? half "left"))
                   (cons (convertChurchChar (stream-car ((idx stream-cdr) (FIRST lats))))
                         (((helper lats) (toChurch 0)) "right")))
                  ((and (convertChurchBool (GEQ? idx (toChurch 0))) (equal? half "left"))
                   (cons (convertChurchChar (stream-car ((idx stream-cdr) (FIRST lats))))
                         (((helper lats) (PRED idx)) "left")))
                  ((and (convertChurchBool (NOT (GEQ? idx (toChurch 11)))) (equal? half "right"))
                   (cons (convertChurchChar (stream-car ((idx stream-cdr) (SECOND lats))))
                         (((helper lats) (SUCC idx)) "right")))
                  (#t '()))))]
      (((helper tape) (toChurch 9)) "left"))))

;----------------------------------------------------------;
;---Functions to print lambda expressions to the console---;
;----------------------------------------------------------;

(define displayHash (hash ; codes for each conversion function
                     "a_bool" convertChurchBool
                     "a_natural" convertChurchNumeral
                     "a_char" convertChurchChar
                     "b_natural pair" (convertChurchPair convertChurchNumeral)
                     "l_natural list" (convertChurchList convertChurchNumeral)
                     "l_char list" (convertChurchList convertChurchChar)
                     "m_natural list list" (convertChurchList (convertChurchList
                                                               convertChurchNumeral))
                     "a_signed" convertSignedChurchNumeral
                     "a_rational" convertRationalChurchNumeral
                     "l_signed list" (convertChurchList convertSignedChurchNumeral)
                     "l_rational list" (convertChurchList convertRationalChurchNumeral)
                     "z_tape section"
                     convertTuringTape))

(provide (rename-out [displaylnConvert displayln!]) displayHash)
(define displaylnConvert ; given a conversion code and an expression, will print it
  (lambda (type)
    (lambda (data)
      (displayln ((hash-ref displayHash type) data)))))

(provide (rename-out [tryDisplayln displaylnAuto!]))
(define tryDisplayln ; trys all conversions in turn until it finds one that works
                     ; (works a decent amount of the time)
  (lambda (data)
    (call/cc (lambda(exit)
               (letrec [(helper
                         (lambda (type)
                           (lambda (data)
                             (try
                              [(begin ((displaylnConvert (car type)) data) (exit))]
                              [catch (cond
                                       ((null? type) (displayln " bad user"))
                                       (#t ((helper (cdr type)) data)))]))))]
                 ((helper (hash-keys displayHash #t)) data))))))

(provide (rename-out [displaylnTape displayTape])) ; for my turing machines
(define displaylnTape
  (lambda (data)
    (displayln (convertTuringTape (FIRST data)))
    (displayln "                     ^                      ")
    (display   "                     ")
    (displayln (convertChurchNumeral (SECOND data)))
    (newline)))

(define-syntax try ; try catch block for the tryDisplayln
  (syntax-rules (catch)
    ((_ body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
        (call-with-exception-handler
         (lambda (condition)
           catcher
           (exit condition))
         (lambda () body)))))))

;----------------------------;
;---Internal Macro Utility---;
;----------------------------;

; a generator macro which is just a simple replace for a lambda calculus expression
(provide define-simple-func)
(define-syntax (define-simple-func stx)
  (syntax-parse stx
    [(_ name:id alt:id func:expr)
     #'(begin
         (provide (rename-out [name alt]))
         (define-syntax (name stx)
           (define-syntax-class alt-names
             #:description "set of alternate names"
             #:datum-literals (name alt)
             (pattern (~or name alt)))

           (... (syntax-parse stx
                  #:literals (lambda)
                  [:alt-names #'func]
                  [(:alt-names argss ...) #'(curry-app func argss ...)]))))]))

;;;=================================Base Language================================;;;
;;; a turing complete/"usable" subset of racket using pure lambda expressions    ;;;
;;;==============================================================================;;;

;---------------------;
;---Let Expressions---;
;---------------------;

(provide (rename-out [let/lc let]))
(define-syntax (let/lc stx) ; works like a let expression
  (syntax-parse stx
    #:literals (lambda)
    [(let/lc ((a b) ...) body:expr ...+)
     #'(curry-app (lambda (a ...) body ...) b ...)]))

(provide (rename-out [let/lc* let*])) ; works like a let* expression
(define-syntax let/lc*
  (syntax-rules ()
    [(_ ((a b) ...) body ...) (curry-for-let* ((lambda (a ...) body ...) b ...))]))

(define-syntax curry-for-let* ; let* helper
  (syntax-rules ()
    [(_ ((lambda (a) body ...) c)) ((lambda (a) body ...) c)]
    [(_ ((lambda (a b ...) body ...) c d ...))
     ((lambda (a) (curry-for-let* ((lambda (b ...) body ...) d ...))) c)]))

;-----------------------------------------------;
;---Convert Scheme Numbers to Church Numerals---;
;-----------------------------------------------;

(provide (rename-out [toChurch $]))
(define-syntax (toChurch stx) ; convert number literal to a church numeral recursivly
  (syntax-parse stx
    #:literals (lambda)
    [(_ num:nat) #'(toChurch num (lambda (f) (lambda (x) x)))]              ; entry case
    [(_ 0 (lambda (f) (lambda (x) body))) #'(lambda (f) (lambda (x) body))] ; base case
    [(_ appsLeft:nat (lambda (f) (lambda (x) body)))                        ; recursive case
     #:with newAppsLeft (datum->syntax #'appsLeft (- (syntax->datum #'appsLeft) 1))
     #'(toChurch newAppsLeft (lambda (f) (lambda (x) (f body))))]))

(provide (rename-out [charToChurch &])) 
(define-syntax (charToChurch stx) ; a character can be represented using its ascii codes
  (syntax-parse stx
    #:literals (lambda)
    [(_ char)                               
     #:with charNum (datum->syntax #'char (char->integer (syntax->datum #'char)))
     #'(toChurch charNum)]))

;---------------;
;---Aritmetic---;
;---------------;

(define-simple-func SUCC add1 (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) )
(define-simple-func ADD  +    (lambda (m) (lambda (n) ((m SUCC) n)))               )
(define-simple-func MULT *    (lambda (m) (lambda (n) ((m (ADD n)) (toChurch 0)))) )
(define-simple-func EXPT expt (lambda (b) (lambda (e) (e b)))                      )

;------------------------------------------------------;
;---Church Booleans, Boolean Algebra, and Predicates---;
;------------------------------------------------------;

(define-simple-func TRUE  $t    (lambda (a) (lambda (b) a))               )
(define-simple-func FALSE $f    (lambda (a) (lambda (b) b))               )
(define-simple-func NOT   not   (lambda (p) ((p FALSE) TRUE))             )
(define-simple-func AND   and   (lambda (p) (lambda (q) ((p q) p)))       )
(define-simple-func OR    or    (lambda (p) (lambda (q) ((p p) q)))       )
(define-simple-func ZERO? zero? (lambda (n) ((n (lambda(x) FALSE)) TRUE)) )

;------------------------------;
;---Pairs and Pair Functions---;
;------------------------------;

(define-simple-func PAIR   cons  (lambda (x) (lambda (y) (lambda (f) ((f x) y)))) )
(define-simple-func FIRST  car   (lambda (p) (p TRUE))                            )
(define-simple-func SECOND cdr   (lambda (p) (p FALSE))                           )
(define-simple-func NIL    null  (lambda (x) TRUE)                                )
(define-simple-func NIL?   null? (lambda (p) (p (lambda (x) (lambda (y) FALSE)))) )

;----------------------;
;---Nice List Syntax---;
;----------------------;

(provide (rename-out (LIST list) (LIST L) (LIST T)))
(define-syntax (LIST stx)
  (syntax-parse stx
    [(_ () pairs) #'pairs]                                                ;base case
    [(_ (arg ... last) pairs) #'(LIST (arg ...) ((PAIR last) pairs))]     ;recursive case
    [(_ arg:expr ... last:expr) #'(LIST (arg ...) (PAIR last NIL))]       ;entry case
    [(_ ()) #'NIL]                                                        ;special case
    [(_ ) #'NIL]))                                                        ;special case

; instead of having to manually chain car and cdrs
(define-simple-func REF list-ref (lambda (n) (lambda (l) (FIRST ((n SECOND) l)))))

;----------------------------;
;---Subtraction Arithmetic---;
;----------------------------;
(define-simple-func Φ    phi  (lambda (p) ((PAIR (SECOND p)) (SUCC (SECOND p))))            )
(define-simple-func PRED sub1 (lambda (n) (FIRST ((n Φ) ((PAIR (toChurch 0)) (toChurch 0)))) ))
(define-simple-func SUB  -    (lambda (m) (lambda (n) ((n PRED) m)))                        )
(define-simple-func GEQ? >=   (lambda (arg1) (lambda (arg2) (ZERO? (SUB arg2 arg1))))       )
(define-simple-func EQ?  =    (lambda (arg1) (lambda (arg2)  (AND (GEQ? arg1 arg2)
                                                                  (GEQ? arg2 arg1))))       )

;--------------------------;
;---Recursion and letrec---;
;--------------------------;

(define-simple-func M      m (lambda (f) (f f)) )
(define-simple-func Y-app  y (lambda (f) (M (lambda (x) (f (lambda (z) ((x x) z)))))))

(provide (rename-out (rec-def rec)))
(define-syntax (rec-def stx)
  (syntax-parse stx
    #:literals (lambda)
    #:datum-literals (Y-app)
    [(_ funcName:id body:expr) #'(Y-app (lambda (funcName) body))]))

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
;              "external" let definition            "internal" recursive definition

(provide with)
(define-syntax (with stx) ; try catch block for the tryDisplayln
  (syntax-parse stx
    #:datum-literals (do define)
    ((with (define a b) ... do body ...) #'(let-rec ((a b) ...) body ...))))

;------------------;
;---Conditionals---;
;------------------;

(provide if)
(define-syntax if ;only eval the executed branch by delaying with lambdas
  (syntax-rules ()
    [(_ pred then else) (thaw ((pred (freeze then)) (freeze else)))]
    [(_ pred then) (thaw ((pred (freeze then)) (freeze FALSE)))]))

(provide (rename-out (mycond cond)))
(define-syntax mycond ; cond is essentially a chain of ifs
  (syntax-rules ()
    ((mycond body ()) (thaw body)) ; base case - execute the chosen body
    ((mycond built ((pred body) ... (lPred lBody))) ; recursive case - lazy evaluation with lambdas
     (mycond ((lPred (freeze lBody)) built) ((pred body) ...)))
    ((mycond (pred body) ... (lPred lBody)) ; entry case - cond syntax
     (mycond ((lPred (freeze lBody)) (freeze FALSE)) ((pred body) ...)))))

(define-simple-func _default default TRUE)
(define-simple-func _else else TRUE)

;--------------------;
;---Division Stuff---;
;--------------------;

(define-simple-func DIV / (lambda (t)
                            (lambda (b)
                              (let-rec ((helper
                                         (lambda (top)
                                           (lambda (bottom)
                                             (lambda (acc)
                                               (mycond
                                                ((ZERO? bottom) (toChurch 0))
                                                ((EQ? top bottom) (SUCC acc))
                                                ((ZERO? top) (PRED acc))
                                                (TRUE (((helper (SUB top bottom))
                                                                 bottom)
                                                                (SUCC acc)))))))))
                                       (((helper t) b) (toChurch 0))))))

(define-simple-func remainder % (rec-def helper (lambda (top)
                                                  (lambda (bottom)
                                                    (mycond
                                                     ((EQ? top bottom) (toChurch 0))
                                                     ((ZERO? (SUB top bottom)) top)
                                                     (TRUE ((helper (SUB top bottom))   
                                                                     bottom)))))))

;------------------;
;---Curry it All---;
;------------------;

;;; So far we have done nothing about multi-argument functions, which technically     ;;;
;;; aren't lambda-calculus-pure. This isn't too much of a problem for most things,    ;;;
;;; but it would be _better_ to have everything curried, and some functions (like the ;;;
;;; applicative Y combinator) will actualy only work with single argument functiions  ;;;

;;; We can curry functions themselves pretty easily: (lambda (a b) body) is ;;;
;;; transformed into (lambda (a) (lambda (b) body)) using a fairly trivial  ;;;
;;; (compared to some others) recursive macro                               ;;;

;;; The other problem is currying application: (func a b) is not transformed into   ;;;
;;; ((func a) b) as trivially since we don't have a constant keyword (like lambda)  ;;;
;;; to anchor the syntax transformer. However, there is actually a "hidden" racket  ;;;
;;; function called #%app, which is auto-inserted by the compiler and does function ;;;
;;; application ((func a b) is actually (#%app func a b)). We can thus export our   ;;;
;;; own redefinition of #%app that automatically curries the application.           ;;;

(provide (rename-out [curry-lambda lambda] [curry-lambda λ])) ; give user auto curry lambdas
(define-syntax (curry-lambda stx)
  (syntax-parse stx
    [(_ (a b ...+) body ...) #'(λ (a) (curry-lambda (b ...) body ...))]
    [(_ (a) body ...)        #'(λ (a) body ...)]
    [(_ () body ...)         #'(λ () body ...)]))

; auto curry every lambda in this module too, can pull this trick because
; the lambda and λ keywords are equivilant, so we can co-opt one of them
(define-syntax (lambda stx)
  (syntax-parse stx
    #:literals (lambda)
    [(lambda stuff ...) #'(curry-lambda stuff ...)]))

;;; Note - keywords and scoping get a wonky here
;;; In this module -
;;;   curry-lambda and lambda are auto currying
;;;   λ is the defualt racket definition
;;; In the users code -
;;;   curry-lambda, lambda, and λ are auto currying
;;;   (they all get reduced to _this module's_ λ definition,
;;;      this is not a naming conflict because of macro hygene (thanks racket!))

(provide (rename-out [curry-app #%app])) ; give the user auto currying applications
(define-syntax (curry-app stx)
  (syntax-parse stx
    [(_ expr arg1 arg2 ...+) #'(curry-app (expr arg1) arg2 ...)]
    [(_ expr ...) #'(expr ...)]))

; cant pull the auto lambda trick since there is only one #%app keyword, any curried
; applications in this module are explicit, but the user gets a custom #%app syntax transformer

;;;===========================Expanded Algebra "Library"=========================;;;
;;; signed and rational numbers and corresponding arithmetic                     ;;;
;;;==============================================================================;;;

;--------------------;
;---Signed Numbers---;
;--------------------;

;;; A signed number is a pair of church numerals where (a, b) represents the number a - b ;;;
;;; This choice is fairly arbitrary but it eliminates a lot of conditionals in operations ;;;

(define-simple-func CONVERTs nat->signed (lambda (x) ((PAIR x) (toChurch 0)))         ) 
(define-simple-func NEGs     neg         (lambda (x) (((PAIR (SECOND x))) (FIRST x))) )

(provide (rename-out (toChurchs $$)))
(define-syntax (toChurchs stx) ; convert a racket literal to a signed number expression
  (syntax-case stx ()
    [(_ num)
     (with-syntax ([newstx
                    (with-syntax [(n (datum->syntax #'num (abs (syntax->datum #'num))))]
                      (cond
                        ((not number?) (error "not signed church numeralable"))
                        ((< (syntax->datum #'num) 0) #'(NEGs (CONVERTs (toChurch n))))
                        (#t #'(CONVERTs (toChurch n)))))])
       #'newstx)]))

(define-syntax (SIMPLIFYs stx) ; simplify a signed number to it's unique representation
                               ; (0, b) or (a, 0)
  (syntax-case stx ()
    [(_ num) #'((rec-def simp
                         (lambda (x)
                           (mycond
                            ((OR (ZERO? (FIRST x)) (ZERO? (SECOND x))) x)
                            (TRUE (simp ((PAIR (PRED (FIRST x))) (PRED (SECOND x)))))))) num)]))

; do arithmetic and comparisons with them
(define-simple-func ADDs  +s (lambda (x)
                               (lambda (y)
                                 (SIMPLIFYs
                                  ((PAIR ((ADD (FIRST x)) (FIRST y)))
                                        ((ADD (SECOND x)) (SECOND y)))))))
(define-simple-func SUBs  -s (lambda (x)
                               (lambda (y)
                                 (SIMPLIFYs
                                  ((PAIR ((ADD (FIRST x)) (SECOND y)))
                                        ((ADD (SECOND x)) (FIRST y)))))))
(define-simple-func MULTs *s (lambda (x)
                               (lambda (y)
                                 (SIMPLIFYs
                                  (PAIR [ADD (MULT (FIRST x) (FIRST y))
                                             (MULT (SECOND x) (SECOND y))]
                                        [ADD (MULT (FIRST x) (SECOND y))
                                             (MULT (SECOND x) (FIRST y))])))))
(define-simple-func DIVs  /s (lambda (x)
                               (lambda (y)
                                 (SIMPLIFYs
                                  ((PAIR [ADD (DIV (FIRST x) (FIRST y))
                                             (DIV (SECOND x) (SECOND y))])
                                        [ADD (DIV (FIRST x) (SECOND y))
                                             (DIV (SECOND x) (FIRST y))])))))
(define-simple-func REMAINDERs %s (lambda (x)
                                    (lambda (y)
                                      (SIMPLIFYs (SUBs x (MULTs y (DIVs x y)))))))
(define-simple-func ZERO?s zero?s (lambda (n) (EQ? (FIRST n) (SECOND n))))
(define-simple-func GEQ?s  >=s    (lambda (arg1)
                                    (lambda (arg2)
                                      (ZERO?s (SUBs arg2 arg1)))))

;---------------;
;---rationals---;
;---------------;

;;; A rational number is a pair of signed numerals where (x, y) represents the ;;;
;;; number x / y. Note - y = 0 is currently undefined, don't do it!            ;;;

(define-simple-func CONVERTr /1 (lambda (x) ((PAIR x) (toChurchs 1)))        ) 
(define-simple-func INVERTr  1/ (lambda (x) ((PAIR (SECOND x)) (FIRST x))) )

(provide (rename-out (toChurchr $$$)))
(define-syntax (toChurchr stx) ; convert a racket literal to a rational number expression
  (syntax-case stx ()
    [(_ num) #'(CONVERTr (toChurchs num))]
    [(_ top bottom) #'(SIMPLIFYr ((PAIR (toChurchs top)) (toChurchs bottom)))]))

(define-simple-func SIMPLIFYr _simplifyr
  (lambda (r) ; simplify a rational number down to it's unique representation
    (let-rec [(gcd
               (lambda (a)
                 (lambda (b)
                   (mycond
                    ((ZERO?s b) a)
                    (TRUE ((gcd b) ((REMAINDERs a) b)))))))
              (theGCD ((gcd (FIRST r)) (SECOND r)))]
             ((PAIR (DIVs (FIRST r) theGCD)) (DIVs (SECOND r) theGCD)))))
(define-simple-func ADDr +r
  (lambda (x)
    (lambda (y)
      (let/lc* [(numer_x (FIRST x))
                (numer_y (FIRST y))
                (denom_x (SECOND x))
                (denom_y (SECOND y))
                (new_denom (MULTs denom_x denom_y))]
               (SIMPLIFYr ((PAIR (ADDs (MULTs numer_x denom_y)
                                       (MULTs numer_y denom_x))) new_denom))))))
(define-simple-func SUBr  -r
  (lambda (x)
    (lambda (y)
      (let/lc* [(numer_x (FIRST x))
                (numer_y (FIRST y))
                (denom_x (SECOND x))
                (denom_y (SECOND y))
                (new_denom (MULTs denom_x denom_y))]
               (SIMPLIFYr ((PAIR (SUBs (MULTs numer_x denom_y)
                                      (MULTs numer_y denom_x))) new_denom))))))
(define-simple-func MULTr *r
  (lambda (x)
    (lambda (y)
      (let/lc* [(numer_x (FIRST x))
                (numer_y (FIRST y))
                (denom_x (SECOND x))
                (denom_y (SECOND y))]
               (SIMPLIFYr ((PAIR (MULTs numer_x numer_y)) (MULTs denom_y denom_x)))))))
(define-simple-func DIVr /r (lambda (x) (lambda (y) (MULTr x (INVERTr y))))) ; 6th grade math trix
(define-simple-func ZERO?r zero?r (lambda (n) (ZERO?s (FIRST n))))

#;(define-simple-func GEQ?r >=r (lambda (arg1) (lambda (arg2) (ZERO?r (SUBr arg2 arg1)))))

;;; Note on number types
;;; rational numbers require signed numbers, so the different types form a bit of a hierarchy/pipeline
;;;   CONVERRTs :: natural -> signed
;;;   CONVERRTr :: signed -> rational
;;; and with a,b,c,d as naturals, x,y as signed numbers, and r as a rational number
;;;   natural number  - a <-> λf.λx.f^a(x)
;;;   signed number   - x <-> (a, b)
;;;   rational number - r <-> (x, y) <-> ((a, b), (c, d))
;;; so to make a rational number we have to make 4 church numerals and 2 signed numerals
;;; the arithmatic starts to get very inefficient, but thats not the focus so it's all good

;;;================================Stream "Library"==============================;;;
;;; lazy computations 'n stuff                                                   ;;;
;;;==============================================================================;;;

;-----------------------;
;---Stream Primatives---;
;-----------------------;

;;; delay and force are already in the racket language, but I like them here,
;;; and I prefer freeze and thaw as more visual names
;;; (we just export as delay and force to match racket though)

(provide (rename-out (freeze delay)))
(define-syntax      freeze (syntax-rules () [(_ x) (lambda () x)]))
(define-simple-func thaw force               (lambda (x) (x)))

(provide (rename-out (stream-cons cons/l)))
(define-syntax stream-cons ; a lazy cons freezes the tail
  (syntax-rules ()
    [(stream-cons x lat) ((PAIR x) (freeze lat))]))

(define-simple-func stream-car car/l (lambda (lat) (FIRST lat))         ) ; just a car
(define-simple-func stream-cdr cdr/l (lambda (lat) (thaw (SECOND lat))) ) ; need the tail now

(define-simple-func _the-empty-stream the-empty-stream NIL  ) ; the empty stream is NIL
(define-simple-func _stream-null?     stream-null?     NIL? )

;------------------------;
;---Nice Stream Syntax---;
;------------------------;

(provide (rename-out (STREAM stream)))
(define-syntax (STREAM stx) ; nice stream syntax (semantically the same as list)
  (syntax-parse stx
    [(_ () pairs) #'pairs]                                                     ;base case
    [(_ (arg ... last) pairs) #'(STREAM (arg ...) ((stream-cons last) pairs))] ;recursive case
    [(_ arg:expr ... last:expr)
     #'(STREAM (arg ...) (stream-cons last the-empty-stream))]                 ;entry case
    [(_ ()) #'the-empty-stream]                                                ;special case
    [(_ ) #'the-empty-stream]))                                                ;special case

;----------------------;
;---Stream Functions---;
;----------------------;

(define-simple-func stream-combine combine/l
  (rec-def combine/lr (lambda (op) ; combine two streams with an operator
                        (lambda (s1)
                          (lambda (s2)
                            (let ((h1 (stream-car s1))
                                  (h2 (stream-car s2)))
                              (stream-cons ((op h1) h2) 
                                           (((combine/lr op) (stream-cdr s1)) (stream-cdr s2)))))))))
(define-simple-func stream-map     map/l
  (rec-def map/l ; map a function over a stream
           (lambda (f)
             (lambda (s1)
               (let ((h1 (stream-car s1)))
                 (mycond
                  ((NIL? s1) NIL)
                  (TRUE (stream-cons (f h1) ((map/l f) (stream-cdr s1))))))))))
(define-simple-func stream-filter  filter/l
  (rec-def filter/l ; filter a stream using a predicate function
           (lambda (f)
             (lambda (stream)
               (let/lc ((item (stream-car stream)))
                       (mycond
                        ((NIL? stream) NIL)
                        ((f item) (stream-cons item ((filter/l f) (stream-cdr stream))))
                        (TRUE ((filter/l f) (stream-cdr stream)))))))))
(define-simple-func stream-reduce  reduce/l
  (rec-def reduce/l ; fold a stream left
           (lambda (f)
             (lambda (zero)
               (lambda (stream)   
                 (let/lc ((item (stream-car stream)))
                         (mycond
                          ((NIL? stream) zero)
                          (TRUE ((f (stream-car stream)) (((reduce/l f) zero) (stream-cdr stream)))))))))))
(define-simple-func stream-limit   limit/l
  (rec-def limit/lr ; limit a stream (make an infinite stream finite to n terms)
           (lambda (n)
             (lambda (s1)
               (let/lc ((h1 (stream-car s1)))
                       (mycond
                        ((NIL? s1) NIL)
                        ((ZERO? n) NIL)
                        (TRUE (stream-cons h1 ((limit/lr (PRED n)) (stream-cdr s1))))))))))
(define-simple-func stream-skip    skip/l
  (rec-def skip/l ; skip the first n elements in a stream
           (lambda (n)
             (lambda (s1)
               (let/lc ((h1 (stream-car s1)))
                       (mycond
                        ((NIL? s1) NIL)
                        ((ZERO? n) s1)
                        (TRUE ((skip/l (PRED n)) (stream-cdr s1)))))))))
(define-simple-func collect-list   collect
  (rec-def collect (lambda (s1) ; collect the stream into a list (stream must not be infinite, use limit if needed)
                     (mycond
                      ((NIL? s1) (LIST))
                      (TRUE (PAIR (stream-car s1) (collect (stream-cdr s1))))))))
(define-simple-func _ps       previewStream!
  (lambda (num)  ; utility function to print the first num elements of a stream
    (lambda (s1)
      (tryDisplayln (stream-reduce PAIR (LIST) ((stream-limit num) s1))))))

;;;===================Tacit Programming and Combinator Playground================;;;
;;; lazy computations 'n functional programming 'n stuff                         ;;;
;;;==============================================================================;;;

(define-simple-func IDENTITY      I  (lambda (x) x)                                                        )
(define-simple-func COMPOSE       o  (lambda (f) (lambda (g) (lambda (x) (f (g x)))))                      ) ; bluebird
(define-simple-func CARDINAL      C  (lambda (f) (lambda (a) (lambda (b) ((f b) a))))                      )
(define-simple-func KESTRAL       K  (lambda (a) (lambda (b) a))                                           )
(define-simple-func SPLIT-COMBINE SR (lambda (r) (lambda (f) (lambda (g) (lambda (n) ((r (f n)) (g n)))))) ) ;phoenix

(provide (rename-out (COMPOSE-MANY o*)))
(define-syntax (COMPOSE-MANY stx)
  (syntax-parse stx
    [(COMPOSE-MANY f g ...+) #'(COMPOSE f (COMPOSE-MANY g ...))]
    [(COMPOSE-MANY f) #'f]
    [(COMPOSE-MANY) #'IDENTITY]))


(provide (rename-out (COMPOSE-MANY-REV *o)))
(define-syntax (COMPOSE-MANY-REV stx)
  (syntax-parse stx
    [(COMPOSE-MANY-REV h ...+ f g) #'(COMPOSE g (COMPOSE-MANY-REV h ... f))]
    [(COMPOSE-MANY-REV f g) #'(COMPOSE g f)]
    [(COMPOSE-MANY-REV f) #'f]
    [COMPOSE-MANY-REV #'IDENTITY]))

(provide (rename-out (point-free-cond tacit-cond)))
(define-syntax point-free-cond
  (syntax-rules ()
    ((point-free-cond body () x) (body x)) ; base case - execute the chosen body
    ((point-free-cond built ((pred body) ... (lPred lBody)) x) ; recursive case - lazy evaluation with lambdas
     (point-free-cond (((lPred x) (lambda (y) (lBody y))) built) ((pred body) ...) x))
    ((point-free-cond (pred body) ... (lPred lBody)) ; entry case - cond syntax
     (lambda (x) (point-free-cond (((lPred x) (lambda (y) (lBody y)))
                                   (lambda (y) FALSE)) ((pred body) ...) x)))))
