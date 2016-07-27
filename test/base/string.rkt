#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit 
         rosette/base/core/term rosette/base/core/string
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         "common.rkt")


(define tests:string?
  (test-suite+
   "Tests for string? in rosette/base/string.rkt"
   (check-string?)
   (check-string-cast)))

; TODO do I need to test string/equal? string/compress? etc

(define tests:string-append
  (test-suite+
   "Tests for string-append in rosette/base/string.rkt"
   (check-string-append-empty)
   (check-string-append-lit)
   (check-string-append-symbolic))) 

(define tests:string-length
  (test-suite+
   "Tests for string-length in rosette/base/string.rkt"
   (check-string-length-empty)
   (check-string-length-lit)
   (check-string-length-symbolic))) 

(define tests:string->integer
  (test-suite+
   "Tests for string->integer in rosette/base/string.rkt"
   (check-string->integer-empty)
   (check-string->integer-non-number)
   (check-string->integer-non-integer)
   (check-string->integer-lit)
   (check-string->integer-symbolic)))

(define tests:integer->string
  (test-suite+
   "Tests for integer->string in rosette/base/string.rkt"
   (check-integer->string-lit)
   (check-integer->string-symbolic))) 

(define tests:substring
  (test-suite+
   "Tests for substring in rosette/base/string.rkt"
   (check-substring-empty)
   (check-substring-negative-i)
   (check-substring-out-of-bounds)
   (check-substring-no-j)
   (check-substring-lit)
   (check-substring-symbolic)))

(define tests:string-contains?
  (test-suite+
   "Tests for string-contains? in rosette/base/string.rkt"
   (check-string-contains?-empty)
   (check-string-contains?-lit)
   (check-string-contains?-symbolic)))

(define tests:string-prefix?
  (test-suite+
   "Tests for string-prefix? in rosette/base/string.rkt"
   (check-string-prefix?-empty)
   (check-string-prefix?-lit)
   (check-string-prefix?-sybmolic))) 

(define tests:string-suffix?
  (test-suite+
   "Tests for string-suffix? in rosette/base/string.rkt"
   (check-string-suffix?-empty)
   (check-string-suffix?-lit)
   (check-string-suffix?-sybmolic)))

(define tests:string-replace
  (test-suite+
   "Tests for string-replace in rosette/base/string.rkt"
   (check-string-replace-empty)
   (check-string-replace-all)
   (check-string-replace-first)
   (check-string-replace-symbolic)))

(define tests:string-at
  (test-suite+
   "Tests for string-at in rosette/base/string.rkt"
   (check-string-at-empty)
   (check-string-at-negative-offset)
   (check-string-at-out-of-bounds)
   (check-string-at-lit)
   (check-string-at-symbolic))) 

(define tests:string-index-of
  (test-suite+
   "Tests for string-index-of in rosette/base/string.rkt"
   (check-string-index-of-empty)
   (check-string-index-of-not-contains)
   (check-string-index-of-negative-offset)
   (check-string-index-of-out-of-bounds)
   (check-string-index-of-no-offset)
   (check-string-index-of-offset)
   (check-string-index-of-symbolic)))

(define tests:string-immutability
  (test-suite+
   "Tests for string immutability in rosette/base/string.rkt"
   (check-immutability string-set! string-fill! string-copy!)))

(time (run-tests tests:string?))
(time (run-tests tests:string-append))
(time (run-tests tests:string-length))
(time (run-tests tests:string->integer))
(time (run-tests tests:integer->string))
(time (run-tests tests:substring))
(time (run-tests tests:string-contains?))
(time (run-tests tests:string-prefix?))
(time (run-tests tests:string-suffix?))
(time (run-tests tests:string-replace))
(time (run-tests tests:string-at))
(time (run-tests tests:string-index-of))
(time (run-tests tests:string-immutability))