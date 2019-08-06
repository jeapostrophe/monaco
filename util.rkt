#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/format
         syntax/parse/define)
(provide (all-defined-out))

(define (show-mem)
  (printf "~a MB\n" (real->decimal-string (/ (current-memory-use) (* 1024 1024)))))

(define (~b x w)
  (~r x #:min-width w #:pad-string "0" #:base 2))

(define (between lo x hi)
  (and (<= lo x) (< x hi)))

(define (random-list-ref l)
  (list-ref l (random (length l))))
(define-syntax-rule (while c e ...)
  (let loop () (when c e ... (loop))))
(define-syntax-rule (until c e ...)
  (while (not c) e ...))
(define-syntax-rule (do-until e c)
  (begin e (until c e)))

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


