#lang racket

(require "term.rkt" "polymorphic.rkt" 
         "real.rkt" "bool.rkt"  
         "merge.rkt" "safe.rkt"
         "string.rkt")

(provide @number? @positive? @negative? @zero? @even? @odd?
         @add1 @sub1 @sgn @truncate @floor @ceiling @min @max
         @exact->inexact @inexact->exact @expt
         @number->string @string->number
         ;@sqrt @bitwise-not @bitwise-and @bitwise-ior @bitwise-xor
         ;@<< @>> @>>> @bitwise-bit-set? @bitwise-bit-field
         )

(define (@number? v)   (or (number? v) (@real? v)))
(define (@positive? x) (@> x 0))
(define (@negative? x) (@< x 0))
(define (@zero? x)     (@= x 0))
(define (@even? x)     (@zero? (@remainder x 2)))
(define (@odd? x)      (! (@even? x)))
(define (@add1 x)      (@+ x 1))
(define (@sub1 x)      (@- x 1))

(define (@sgn x)
  (if (number? x) 
      (sgn x)
      (merge* (cons (@positive? x) 1)
              (cons (@negative? x) -1)
              (cons (@zero? x) 0))))

(define ($truncate x)
  (match x
    [(? real?) (truncate x)]
    [(term _ (== @integer?)) x]
    [(term _ (== @real?)) 
     (let ([xi (@real->integer x)])
       (merge (@< x 0) (@+ xi 1) xi))]))

(define @truncate (lift-op $truncate))

(define (@floor x) (@real->integer x))

(define ($ceiling x)
  (match x
    [(? real?) (ceiling x)]
    [(term _ (== @integer?)) x]
    [(term _ (== @real?))
     (let* ([xi (@real->integer x)])
       (merge (@<= x xi) xi (@+ xi 1)))]))

(define @ceiling (lift-op $ceiling))

(define extreme 
  (case-lambda 
    [(op x) x]
    [(op x y) (merge (op x y) x y)]
    [(op x y . z) (apply extreme op (extreme op x y) z)]))

(define @min (lift-op (curry extreme @<=)))
(define @max (lift-op (curry extreme @>=)))

(define (@exact->inexact x)
  (if (number? x) 
      (exact->inexact x)
      (numeric-coerce x 'exact->inexact)))

(define (@inexact->exact x)
  (if (number? x) 
      (inexact->exact x)
      (numeric-coerce x 'inexact->exact)))

(define (@expt z w)
  (match* (z w)
    [((? number?) (? number?)) (expt z w)]
    [(_ 0) 
     (assert (@real? z) (arguments-error 'expt "expected a number?" "z" z))
     1]
    [(_ (? integer?))
     (if (positive? w) 
         (apply @* (make-list w z))
         (@/ 1 (@* (make-list (- w) z))))]
    [(_ _) (expt z w)]))

; string->number and number->string only support integers and radix 10 for now
; (TODO see if possible to support reals, other radixes)
; TODO simplifications
(define ($string->number s [radix 10])
  (match s
    [(? string?) (string->number s)]
    [_
     (let ([i (@string->integer s)])
       (if (and (@= i 0) (@! (@string/equal? s "0"))) #f i))]))

(define ($guarded-string->number s [radix 10])
  (assert (@= radix 10) (arguments-error 'string->number "expected 10" "radix" radix))
  (let ([n ($string->number s)])
    (assert
     (or (@integer? n) (@boolean? n))
     (arguments-error 'string->number "expected a string? representation of an integer?" "s" s))
    n))

(define-operator @string->number
  #:identifier 'string->number
  #:range (const (or/c @number? @boolean?))
  #:unsafe $string->number
  #:safe
  (lambda (s [radix 10])
    ($guarded-string->number
     (type-cast @string? s 'string->number)
     (type-cast @integer? radix 'string->number))))

(define ($number->string n [radix 10])
  (match n
    [(? integer?) (number->string n radix)]
    [_ (@integer->string n)]))

(define ($guarded-number->string n [radix 10])
  (assert (@= radix 10) (arguments-error 'number->string "expected 10" "radix" radix))
  ($number->string n radix))

(define-operator @number->string
  #:identifier 'number->string
  #:range T*->string?
  #:unsafe $number->string
  #:safe
  (lambda (n [radix 10])
    ($guarded-number->string
     (type-cast @integer? n 'number->string)
     (type-cast @integer? radix 'number->string))))

