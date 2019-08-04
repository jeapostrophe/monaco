#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/format
         syntax/parse/define)
(provide (all-defined-out))

(define (~b x w)
  (~r x #:min-width w #:pad-string "0" #:base 2))

(define-syntax (condlet stx)
  (syntax-parse stx
    [(_)
     #'(void)]
    [(_ [#:cond question answer ...+] . more)
     #'(if question (let () answer ...) (condlet . more))]
    [(_ code)
     #'code]
    [(_ code . more)
     #'(let () code (condlet . more))]))

(define (bit m)
  (arithmetic-shift 1 m))
(define (bitwise-bit-set n m)
  (bitwise-ior n (bit m)))
(define (bitwise-bit-flip n m)
  (bitwise-xor n (bit m)))

(define-simple-macro
  (define-functor (name:id input0 inputN:id ...)
    (define output:id e:expr) ...)
  #:with (this-output ...) (generate-temporaries #'(output ...))
  (define-simple-macro (name input0 inputN ...)
    (~@ #:with this-output (datum->syntax #'input0 'output))
    ...
    (begin
      (define output e) ...
      (define this-output output) ...)))


