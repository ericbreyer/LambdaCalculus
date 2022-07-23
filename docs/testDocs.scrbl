#lang scribble/manual

@(require ;"utils.rkt"
          "../schemeSyntax.rkt"
          (for-syntax racket)
          (for-label scribble/manual-struct
                     "../schemeSyntax.rkt"
                     ;racket
                     version/utils
                     syntax/quote
                     ))

@defmodule["../schemeSyntax.rkt"]
@title{Lambda Calculus Scheme}

@section{Natural Numbers}

@defproc[#:kind "syntax" #:link-target? "toChurch" (toChurch [n  (number? (not negative?))])
         churchNum?]{
  Encodes a natural number using the church numeral encoding.
}
@defproc[#:kind "syntax" ($ [n (number? (not negative?))])
         churchNum?]{
                     Short alias for @racket[toChurch]}

@racketblock[($ 3)]

Will expand to

@racketblock[(lambda (f) (lambda (x) (f (f (f x)))))]

@racket[n] must be a positive integer, else a compile-time error will be thrown

@defproc[#:kind "syntax"(SUCC [n churchNum?])
         churchNum?]{
  Returns the successor (+1) of a church numeral
}

@defproc[#:kind "syntax"(add1 [n churchNum?])
         churchNum?]{
  "scheme-style" alias for @racket[SUCC]
}

@racketblock[(add1 ($ 3))]

Will expand to

@racketblock[(lambda (f) (lambda (x) (f (f (f (f x))))))]

@defproc[#:kind "syntax"(PRED [n churchNum?])
         churchNum?]{
  Returns the predecessor (-1) of a church numeral.
  Note also:
 @racketblock[(PRED ($ 0)) => ($ 0)] 
}

@defproc[#:kind "syntax"(sub1 [n churchNum?])
         churchNum?]{
  "scheme-style" alias for @racket[PRED]
}

@racketblock[(sub1 ($ 3))]

Will expand to

@racketblock[(lambda (f) (lambda (x) (f (f x))))]


@defproc[#:kind "syntax"(ADD [m churchNum?] [n churchNum?])
         churchNum?]{
  Returns the sum, m + n, of two church numerals
}

@defproc[#:kind "syntax"(+ [m churchNum?] [n churchNum?])
         churchNum?]{
  "scheme-style" alias for @racket[ADD]
}

@racketblock[(+ ($ 3) ($ 4))               
             ==syntax==> (((lambda (m) (lambda (n) ((m (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))) n)))
                           (lambda (f) (lambda (x) (f (f (f x)))))) (lambda (f) (lambda (x) (f (f (f (f x)))))))                                                                                  
             ==eval==> (lambda (f) (lambda (x) (f (f (f (f (f (f (f x)))))))))
             ==equiv==> ($ 7)]

@section{Boolean Numbers}