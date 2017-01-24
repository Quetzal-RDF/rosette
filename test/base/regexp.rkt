#lang racket

; TODO work in progress

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/solver/solution
         rosette/base/core/term rosette/base/core/string
         rosette/base/core/regexp
         rosette/base/core/bool
         rosette/base/core/merge
         rosette/base/core/polymorphic
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/equality @equal?)
         (only-in rosette evaluate)
         "common.rkt" "solver.rkt")

(define-symbolic a b c d e f g @boolean?)
(define-symbolic x y z @regexp?)

(define (check-regexp?)
  (check-equal? (@regexp? #rx"$.^") #t)
  (check-equal? (@regexp? #rx".$") #t)
  (check-equal? (@regexp? #rx"foo") #t)
  (check-equal? (@regexp? #rx"[a-z]+[A-Z]*") #t)
  (check-equal? (@regexp? 1) #f)
  (check-equal? (@regexp? "foo") #f)
  (check-equal? (@regexp? (@regexp "foo")) #t)
  (check-equal? (@regexp? x) #t)
  (check-equal? (@regexp? (merge a x #f)) a)
  (check-equal? (@regexp? a) #f)
  (check-equal? (@regexp? (merge a b #f)) #f))

(define (check-regexp-cast)
  (check-cast (@regexp? #rx"$.^") (#t #rx"$.^"))
  (check-cast (@regexp? #rx".$") (#t #rx".$"))
  (check-cast (@regexp? #rx"foo") (#t #rx"foo"))
  (check-cast (@regexp? "foo") (#f "foo"))
  (check-cast (@regexp? 1) (#f 1))
  (check-cast (@regexp? x) (#t x))
  (check-cast (@regexp? (merge a x #f)) (a x))
  (check-cast (@regexp? (merge a b #f)) (#f (merge a b #f))))

(define (check-=-simplifications)
  (check-valid? (@regexp/equal? #rx"foo" #rx"foo") #t)
  (check-valid? (@regexp/equal? #rx"[a-z][A-Z]+" #rx"[a-z][A-Z]+") #t)
  (check-valid? (@regexp/equal? x x) #t)
  (check-valid? (@regexp/equal? (ite a #rx"foo" #rx"foo") (ite b #rx"foo" #rx"foo")) #t))
  ; The following are blocked by https://github.com/Z3Prover/z3/issues/876:
  ;(check-valid? (@regexp/equal? #rx"foo" #rx"foo*") #f)
  ;(check-valid? (@regexp/equal? (ite b #rx"foo" #rx"foo*") #rx"foo") b)
  ;(check-valid? (@regexp/equal? (ite b #rx"foo" #rx"foo*") #rx"foo*") (! b))
  ;(check-valid? (@regexp/equal? (ite b #rx"foo" #rx"foo*") #rx"bar") #f)
  ;(check-valid? (@regexp/equal? #rx"foo" (ite b #rx"foo" #rx"foo*")) b)
  ;(check-valid? (@regexp/equal? #rx"foo*" (ite b #rx"foo" #rx"foo*")) (! b))
  ;(check-valid? (@regexp/equal? #rx"bar" (ite b #rx"foo" #rx"foo*")) #f))
  ;(check-valid? (@regexp/equal? (ite a #rx"foo" #rx"foo*") (ite b #rx"[a-z][0-9]+" #rx"bar")) #f))
  ;(check-valid? (@regexp/equal? (ite a #rx"foo" #rx"foo*") (ite b #rx"foo" #rx"[a-z][0-9]+")) (&& a b))
  ;(check-valid? (@regexp/equal? (ite a #rx"foo" #rx"foo*") (ite b #rx"[a-z][0-9]+" #rx"foo")) (&& a (! b)))
  ;(check-valid? (@regexp/equal? (ite a #rx"foo" #rx"foo*") (ite b #rx"foo*" #rx"[a-z][0-9]+")) (&& (! a) b))
  ;(check-valid? (@regexp/equal? (ite a #rx"foo" #rx"foo*") (ite b #rx"[a-z][0-9]+" #rx"foo*")) (&& (! a) (! b))))

(define tests:regexp?
  (test-suite+
   "Tests for regexp? in rosette/base/regexp.rkt"
   (check-regexp?)
   (check-regexp-cast)
   (check-=-simplifications)))

(time (run-tests tests:regexp?))

(solver-shutdown (solver))