#lang racket

(require rackunit
         rosette/base/core/term rosette/base/core/merge rosette/base/core/union rosette/base/core/bool)
(provide (all-defined-out))

(define (check-nary op id x y z)
  (check-equal? (op) id)
  (check-equal? (op id id) id)
  (check-equal? (op id id id) id)
  (check-equal? (op x) x)
  (check-equal? (op id x) x)
  (check-equal? (op x id) x)
  (check-equal? (op x id y) (op x y))
  (check-equal? (op x y) (op y x))
)

(define-syntax-rule (check-term? val (op arg ...))
  (check-true (match val 
                [(expression (== op) (== arg) ...) #t]
                [_ #f])))

(define-syntax-rule (check-union? r {(g v) ...})
  (check-true (match r 
                [(union : ((== g) (== v)) ...) #t]
                [_ #f])))

(define-syntax-rule (check-match? val pat)
  (check-true (match val 
                [pat #t]
                [_ #f])))

(define-syntax-rule (check-cast (type val) (accepted? result))
  (with-handlers ([exn:fail? (lambda (e) (check-equal? accepted? #f))])  
    (let-values ([(actual-result asserts) (with-asserts (type-cast type val))])
      (check-equal? actual-result result)
      (match asserts
        [(list)   (check-equal? accepted? #t)]
        [(list v) (check-equal? accepted? v)]
        [_ (fail "found more than 1 assertion")]))))

(define-syntax-rule (check-state actual expected-value expected-asserts)
  (let-values ([(v a) (with-asserts actual)])
    (check-equal? v expected-value)
    (check-equal? (apply set a) (apply set expected-asserts))))

