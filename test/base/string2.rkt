#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/solver/solution
         rosette/base/core/term rosette/base/core/string
         rosette/base/core/bool rosette/base/core/real
         rosette/base/core/merge
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/equality @equal?)
         "common.rkt" "solver.rkt")

(define-symbolic a b c d e f g @boolean?)
(define-symbolic xr yr zr @real?)
(define-symbolic xi yi zi @integer?)
(define-symbolic x y z @string?)

; TODO duplicate logic from real.rkt tests, refactor 
(define-syntax-rule (check-cast (type val) (accepted? result))
  (with-handlers ([exn:fail? (lambda (e) (check-equal? accepted? #f))])  
    (let-values ([(actual-result asserts) (with-asserts (type-cast type val))])
      (check-equal? actual-result result)
      (match asserts
        [(list)   (check-equal? accepted? #t)]
        [(list v) (check-equal? accepted? v)]
        [_ (fail "found more than 1 assertion")]))))

; TODO duplicate logic from real.rkt tests, refactor
(define-syntax-rule (check-valid? (op e ...) expected)
  (let ([actual (op e ...)])
    (check-equal? actual expected) 
    ;(printf "ASSERTS: ~a\n" (asserts))
    (define preconditions (asserts))
    (clear-asserts!)
    (check-pred unsat? (apply solve (! (@equal? (expression op e ...) expected)) preconditions))))

; TODO duplicate logic from real.rkt tests, refactor
(define-syntax check-exn
  (syntax-rules ()
    [(_ expr)
     (check-exn exn:fail? (thunk (with-asserts-only expr)))]
    [(_ pred expr)
     (check-exn pred (thunk (with-asserts-only expr)))]))

; TODO duplicate logic from real.rkt tests, refactor
(define-syntax-rule (check-state actual expected-value expected-asserts)
  (let-values ([(v a) (with-asserts actual)])
    (check-equal? v expected-value)
    (check-equal? (apply set a) (apply set expected-asserts))))

(define (check-string?)
  (check-equal? (@string? "") #t)
  (check-equal? (@string? "foo") #t)
  (check-equal? (@string? 1) #f)
  (check-equal? (@string? x) #t)
  (check-equal? (@string? (@integer->string xi)) #t)
  (check-equal? (@string? (merge a x #f)) a)
  (check-equal? (@string? a) #f)
  (check-equal? (@string? (merge a b #f)) #f))

(define (check-string-cast)
  (check-cast (@string? "") (#t ""))
  (check-cast (@string? "foo") (#t "foo"))
  (check-cast (@string? 1) (#f 1))
  (check-cast (@string? x) (#t x))
  (check-cast (@string? (merge a x #f)) (a x))
  (check-cast (@string? (merge a b #f)) (#f (merge a b #f))))

(define (check-string-append-no-args)
  (check-state (@string-append) "" (list)))

(define (check-string-append-empty)
  (check-valid? (@string-append "" x) x)
  (check-valid? (@string-append x "") x)
  (check-valid? (@string-append "" x y) (@string-append x y))
  (check-valid? (@string-append x "" y) (@string-append x y))
  (check-valid? (@string-append x y "") (@string-append x y)))

(define (check-string-append-types)
  (check-exn #px"expected string?" (@string-append 'a))
  (check-exn #px"expected string?" (@string-append "foo" xi))
  (check-exn #px"expected string?" (@string-append (merge a 2 #f))))

(define (check-string-append-lit)
  (check-state (@string-append "foo") "foo" (list))
  (check-state (@string-append "foo" "bar") "foobar" (list))
  (check-state (@string-append "foo" "bar" "baz") "foobarbaz" (list)))

(define (check-string-append-symbolic)
  (check-state (@string-append x) (@string-append x) (list))
  (check-state (@string-append x y) (@string-append x y) (list))
  (check-state (@string-append x y z) (@string-append x y z) (list))
  (check-state (@string-append (merge a x #f)) (@string-append x) (list a))
  (check-state (@string-append (merge a x #f) y) (@string-append x y) (list a))
  (check-state (@string-append x (merge a y #f)) (@string-append x y) (list a))
  (check-state (@string-append (merge a x #f) (merge b y #f)) (@string-append x y) (list a b)))

(define tests:string?
  (test-suite+
   "Tests for string? in rosette/base/string.rkt"
   (check-string?)
   (check-string-cast)))

; TODO do I need to test string/equal? string/compress? etc

; TODO test that strings are immutable

(time (run-tests tests:string?))
;(time (run-tests tests:string-append))
;(time (run-tests tests:string-length))
;(time (run-tests tests:string->integer))
;(time (run-tests tests:integer->string))
;(time (run-tests tests:substring))
;(time (run-tests tests:string-contains?))
;(time (run-tests tests:string-prefix?))
;(time (run-tests tests:string-suffix?))
;(time (run-tests tests:string-replace))
;(time (run-tests tests:string-at))
;(time (run-tests tests:string-index-of))

(solver-shutdown (solver))