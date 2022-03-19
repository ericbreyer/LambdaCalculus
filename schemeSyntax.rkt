#lang racket
(provide(all-defined-out))
;;;  Converting to console output
 (define convertChurchBool
   (lambda (bool)
     ((bool #t) #f)))
 
(define convertChurchNumeral
  (lambda(churchNum)
    ((churchNum (lambda(x) (+ x 1))) 0)))
;
(define convertChurchList
  (lambda (lat)
    (cond
      ((convertChurchBool (NIL? lat)) '())
      (#t (cons (convertChurchNumeral (FIRST lat)) (convertChurchList (SECOND lat)))))))

(define convertChurchPair
  (lambda (p)
    (cons (convertChurchNumeral (FIRST p)) (convertChurchNumeral (SECOND p)))))

;(define convertSignedChurchNumeral
;  (lambda(churchNum)
;    (- (convertChurchNumeral (FIRST churchNum)) (convertChurchNumeral (SECOND churchNum)))))


;;; defining scheme in scheme

(define-syntax let
  (syntax-rules ()
    [(_ ((a b) ...) body ...) (curry ((lambda (a ...) body ...) b ...))]))

(define-syntax curry
  (syntax-rules ()
    [(_ ((lambda (a) body ...) c)) ((lambda (a) body ...)c)]
    [(_ ((lambda (a b ...) body ...) c d ...)) ((lambda (a) (curry ((lambda (b ...) body ...) d ...))) c)]))

;; Numbers

(define-syntax (toChurch stx)
  (syntax-case stx ()
    [(_ num) #'(toChurch num (lambda (f) (lambda (x) x)))]
    [(_ 0 (_ (f) (_ (x) body))) #'(lambda (f) (lambda (x) body))]
    [(_ num (_ (f) (_ (x) body)))
     (with-syntax ([newnum (datum->syntax #'num
                                          (- (syntax->datum #'num) 1))])
     #'(toChurch newnum (lambda (f) (lambda (x) (f body)))))]))

(define-syntax $
  (syntax-rules ()
    [($ arg) (toChurch arg)]))

(define-syntax (SUCC stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))arg)]
    [SUCC #'(lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))]))

(define-syntax (ADD stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (m) (lambda (n) ((m SUCC) n))) arg1)]
    [(_ arg1 arg2) #'(((lambda (m) (lambda (n) ((m SUCC) n)))arg1)arg2)]
    [ADD #'(lambda (m) (lambda (n) ((m SUCC) n)))]))

(define-syntax (MULT stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (m) (lambda (n) ((m (ADD n)) (toChurch 0))))arg1)]
    [(_ arg1 arg2) #'(((lambda (m) (lambda (n) ((m (ADD n)) (toChurch 0))))arg1)arg2)]
    [MULT #'(lambda (m) (lambda (n) ((m (ADD n)) (toChurch 0))))]))

(define-syntax (EXP stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (b) (lambda (e) (e b)))arg1)]
    [(_ arg1 arg2) #'(((lambda (b) (lambda (e) (e b)))arg1)arg2)]
    [EXP #'(lambda (b) (lambda (e) (e b)))]))

;;bools
(define-syntax (TRUE stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (a) (lambda (b) a))arg1)]
    [(_ arg1 arg2) #'(((lambda (a) (lambda (b) a))arg1)arg2)]
    [TRUE #'(lambda (a) (lambda (b) a))]))

(define-syntax (FALSE stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (a) (lambda (b) b))arg1)]
    [(_ arg1 arg2) #'(((lambda (a) (lambda (b) b))arg1)arg2)]
    [FALSE #'(lambda (a) (lambda (b) b))]))

(define-syntax (NOT stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (p) ((p FALSE) TRUE))arg)]
    [NOT #'(lambda (p) ((p FALSE) TRUE))]))

(define-syntax (AND stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (p) (lambda (q) ((p q) p)))arg1)]
    [(_ arg1 arg2) #'(((lambda (p) (lambda (q) ((p q) p)))arg1)arg2)]
    [AND #'(lambda (p) (lambda (q) ((p q) p)))]))

(define-syntax (OR stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (p) (lambda (q) ((p p) q)))arg1)]
    [(_ arg1 arg2) #'(((lambda (p) (lambda (q) ((p p) q)))arg1)arg2)]
    [OR #'(lambda (p) (lambda (q) ((p p) q)))]))

(define-syntax (PAIR stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (x) (lambda (y) (lambda (f) ((f x) y))))arg1)]
    [(_ arg1 arg2) #'(((lambda (x) (lambda (y) (lambda (f) ((f x) y))))arg1)arg2)]
    [(_ arg1 arg2 func) #'((((lambda (x) (lambda (y) (lambda (f) ((f x) y))))arg1)arg2)func)]
    [PAIR #'(lambda (x) (lambda (y) (lambda (f) ((f x) y))))]))

(define-syntax (FIRST stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (p) (p TRUE))arg)]
    [FIRST #'(lambda (p) (p TRUE))]))

(define-syntax (SECOND stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (p) (p FALSE))arg)]
    [SECOND #'(lambda (p) (p FALSE))]))

(define-syntax (NIL stx)
  (syntax-case stx ()
    [NIL #'(lambda (x) TRUE)]))

(define-syntax (NIL? stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (p) (p (lambda (x) (lambda (y) FALSE))))arg)]
    [NIL? #'(lambda (p) (p (lambda (x) (lambda (y) FALSE))))]))

(define-syntax LIST
  (syntax-rules ()
    [(_ (arg ... last) pairs) (LIST (arg ...) ((PAIR last) pairs))]
    [(_ () pairs) pairs]
    [(_ (arg ... last)) (LIST (arg ...) (PAIR last NIL))]
    ))

(define-syntax (Φ stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (p) ((PAIR (SECOND p)) (SUCC (SECOND p))))arg)]
    [Φ #'(lambda (p) ((PAIR (SECOND p)) (SUCC (SECOND p))))]))

(define-syntax (PRED stx)
  (syntax-case stx ()
    [(_ arg) #'((lambda (n) (FIRST ((n Φ) ((PAIR ($ 0)) ($ 0)))))arg)]
    [PRED #'(lambda (n) (FIRST ((n Φ) ((PAIR ($ 0)) ($ 0)))))]))

(define-syntax (SUB stx)
  (syntax-case stx ()
    [(_ arg1) #'((lambda (m) (lambda (n) ((n PRED) m))) arg1)]
    [(_ arg1 arg2) #'(((lambda (m) (lambda (n) ((n PRED) m)))arg1)arg2)]
    [SUB #'(lambda (m) (lambda (n) ((n PRED) m)))]))

(define-syntax M
  (syntax-rules ()
    ((_ func) ((lambda (f) (f f)) func))))

(define-syntax Y
  (syntax-rules ()
    [(_ func) ((lambda (f) (M (lambda (x) (f (lambda () (x x)))))) func)]))

(define-syntax ZERO?
  (syntax-rules ()
    [(_ num) ((lambda (n) ((n (lambda(x) FALSE)) TRUE)) num)]))
