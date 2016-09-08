#lang racket

(require rackunit
         "common.rkt"
         rosette/solver/smt/z3 rosette/solver/solver rosette/solver/solution
         rosette/base/core/term rosette/base/core/bool
         (only-in rosette/base/core/equality @equal?))

(provide solver solve solver-shutdown check-valid?)

(define solver (make-parameter (z3)))

(define (solve  . asserts)
  (solver-assert (solver) asserts)
  (begin0
    (solver-check (solver))
    (solver-clear (solver))))

(define-syntax-rule (check-valid? (op e ...) expected)
  (let ([actual (op e ...)])
    (check-equal? actual expected) 
    ;(printf "ASSERTS: ~a\n" (asserts))
    (define preconditions (asserts))
    (clear-asserts!)
    (check-pred unsat? (apply solve (! (@equal? (expression op e ...) expected)) preconditions))))