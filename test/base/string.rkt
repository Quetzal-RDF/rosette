#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/solver/solution
         rosette/base/core/term rosette/base/core/string
         rosette/base/core/bool
         rosette/base/core/merge
         rosette/base/core/polymorphic
         (only-in rosette/base/core/real @integer? @int? @real? @real->integer @= @<= @>= @< @> @+)
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/equality @equal?)
         (only-in rosette evaluate)
         "common.rkt" "solver.rkt")

(define-symbolic a b c d e f g @boolean?)
(define-symbolic xr yr zr @real?)
(define-symbolic xi yi zi @integer?)
(define-symbolic x y z @string?)

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

(define (check-=-simplifications)
  (check-valid? (@string/equal? "foo" "foo") #t)
  (check-valid? (@string/equal? "foo" "bar") #f)
  (check-valid? (@string/equal? x x) #t)
  (check-valid? (@string/equal? (ite b "foo" "bar") "foo") b)
  (check-valid? (@string/equal? (ite b "foo" "bar") "bar") (! b))
  (check-valid? (@string/equal? (ite b "foo" "bar") "baz") #f)
  (check-valid? (@string/equal? "foo" (ite b "foo" "bar")) b)
  (check-valid? (@string/equal? "bar" (ite b "foo" "bar")) (! b))
  (check-valid? (@string/equal? "baz" (ite b "foo" "bar")) #f)
  (check-valid? (@string/equal? (ite a "foo" "foo") (ite b "foo" "foo")) #t)
  (check-valid? (@string/equal? (ite a "foo" "bar") (ite b "baz" "")) #f)
  (check-valid? (@string/equal? (ite a "foo" "bar") (ite b "foo" "baz")) (&& a b))
  (check-valid? (@string/equal? (ite a "foo" "bar") (ite b "baz" "foo")) (&& a (! b)))
  (check-valid? (@string/equal? (ite a "foo" "bar") (ite b "bar" "baz")) (&& (! a) b))
  (check-valid? (@string/equal? (ite a "foo" "bar") (ite b "baz" "bar")) (&& (! a) (! b))))

(define (check-string-append-no-args)
  (check-state (@string-append) "" (list)))

(define (check-string-append-simplifications)
  (check-valid? (@string-append "" x) x)
  (check-valid? (@string-append x "") x)
  (check-valid? (@string-append "" x y) (@string-append x y))
  (check-valid? (@string-append x "" y) (@string-append x y))
  (check-valid? (@string-append x y "") (@string-append x y)))

(define (check-string-append-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-append 'a))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-append "foo" xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-append (merge a 2 #f))))))

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

(define (check-string-length-empty)
  (check-state (@string-length "") 0 (list)))

(define (check-string-length-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-length 'a))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-length xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-length (merge a 2 #f))))))

(define (check-string-length-lit)
  (check-state (@string-length "a") 1 (list))
  (check-state (@string-length "foo") 3 (list)))

(define (check-string-length-symbolic)
  (check-state (@string-length x) (@string-length x) (list))
  (check-state (@string-length (merge a x #f)) (@string-length x) (list a)))

(define (check-string->integer-simplifications) ; Broken by Z3
  (check-valid? (@string->integer (@integer->string xi)) xi))

(define (check-string->integer-empty)
  (check-state (@string->integer "") 0 (list)))
  
(define (check-string->integer-non-number)
  (check-state (@string->integer "foo") 0 (list))
  (check-state (@string->integer "foo1") 0 (list)))

(define (check-string->integer-non-integer)
  (check-state (@string->integer "1.2") 0 (list)))

(define (check-string->integer-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string->integer 'a))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string->integer xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string->integer (merge a 2 #f))))))

(define (check-string->integer-lit)
  (check-state (@string->integer "1") 1 (list))
  (check-state (@string->integer "123") 123 (list)))

(define (check-string->integer-symbolic)
  (check-state (@string->integer x) (@string->integer x) (list))
  (check-state (@string->integer (merge a x #f)) (@string->integer x) (list a)))

(define (check-integer->string-simplifications)  
  (check-valid? (@integer->string (@string->integer (@integer->string xi))) (@integer->string xi))) 

(define (check-integer->string-types)
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@integer->string 'a))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@integer->string 1.2))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@integer->string x))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@integer->string (merge a "" #f))))))

(define (check-integer->string-lit)
  (check-state (@integer->string -1) "-1" (list))
  (check-state (@integer->string 0) "0" (list))
  (check-state (@integer->string 123) "123" (list)))

(define (check-integer->string-symbolic)
  (check-state (@integer->string xi) (@integer->string xi) (list))
  (check-state (@integer->string xr) (@integer->string (@real->integer xr)) (list (@int? xr)))
  (check-state (@integer->string (merge a xi #f)) (@integer->string xi) (list a))
  (check-state (@integer->string (merge a xr #f)) (@integer->string (@real->integer xr)) (list (&& a (@int? xr)))))

(define (check-substring-empty)
  (check-state (@substring "" 0) "" (list)))

(define (check-substring-simplifications)
  (check-valid? (@substring x 0 0) "")
  (check-valid? (@substring x 0 (@string-length x)) x))

(define (check-substring-negative-i)
  (check-equal? (@substring x xi yi) (@substring x xi yi))
  (define preconditions (asserts))
  (clear-asserts!)
  (check-pred unsat? (solve (@<= xi -1) (@equal? y (@substring x xi)) (first preconditions)))
  (check-pred unsat? (solve (@equal? y (@substring x xi yi)) (@<= xi -1) (first preconditions)))
  (clear-asserts!))

(define (check-substring-out-of-bounds)
  (check-equal? (@substring x xi yi) (@substring x xi yi))
  (define preconditions (asserts))
  (clear-asserts!)
  (check-pred unsat? (solve (@equal? y (@substring x xi yi)) (@> xi yi) (first preconditions)))
  (check-pred unsat? (solve (@equal? y (@substring x xi yi)) (@> yi (@string-length x)) (first preconditions)))
  (clear-asserts!))

(define (check-substring-no-j)
  (check-equal? (@substring x xi) (@substring x xi (@string-length x)))
  (clear-asserts!))

(define (check-substring-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@substring 'a))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@substring xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@substring (merge a 2 #f)))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@substring x y))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@substring x xi y)))))

(define (check-substring-lit)
  (check-state (@substring "foo" 0) "foo" (list))
  (check-state (@substring "foo" 0 1) "f" (list))
  (check-state (@substring "foo" 0 2) "fo" (list))
  (check-state (@substring "foo" 0 3) "foo" (list)))

(define (check-substring-symbolic)
  (check-state (@substring "foo" xi) (@substring "foo" xi) (list (&& (@<= 0 xi) (@<= xi 3))))
  (clear-asserts!)
  (check-state (@substring "foo" 0 xi) (@substring "foo" 0 xi) (list (&& (@<= 0 xi) (@<= xi 3))))
  (clear-asserts!)
  (check-state (@substring "foo" xi yi) (@substring "foo" xi yi) (list (&& (@<= 0 xi) (@<= xi yi) (@<= yi 3))))
  (clear-asserts!)
  (check-state (@substring x 0) (@substring x 0) (list (@<= 0 (@string-length x))))
  (clear-asserts!)
  (check-state (@substring x 0 1) (@substring x 0 1) (list (@<= 1 (@string-length x))))
  (clear-asserts!)
  (check-state (@substring x xi) (@substring x xi) (list (&& (@<= 0 xi) (@<= xi (@string-length x)))))
  (clear-asserts!)
  (check-state (@substring x xi 1) (@substring x xi 1) (list (&& (@<= 0 xi) (@<= xi 1) (@<= 1 (@string-length x)))))
  (clear-asserts!)
  (check-state (@substring x 0 xi) (@substring x 0 xi) (list (&& (@<= 0 xi) (@<= xi (@string-length x)))))
  (clear-asserts!)
  (check-state (@substring (merge a x #f) (merge b xi #f) (merge c yi #f)) (@substring x xi yi) (list (&& (@<= 0 xi) (@<= xi yi) (@<= yi (@string-length x))) a b c)))

(define (check-string-contains?-simplifications)
  (check-valid? (@string-contains? x "") #t)
  (check-valid? (@string-contains? "" x) (@equal? x ""))
  ; Z3 cannot yet handle unbounded lengths for this query; see https://github.com/Z3Prover/z3/issues/812
  (@assert (@<= (@string-length x) 5))
  (check-valid? (@string-contains? x (@substring x xi yi)) #t)
  (clear-asserts!)) 

(define (check-string-contains?-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-contains? 'a ""))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-contains? "" xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-contains? (merge a 2 #f) x))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-contains? x (merge a xi #f))))))

(define (check-string-contains?-lit)
  (check-state (@string-contains? "foo" "f") #t (list))
  (check-state (@string-contains? "foo" "o") #t (list))
  (check-state (@string-contains? "foo" "fo") #t (list))
  (check-state (@string-contains? "foo" "oo") #t (list))
  (check-state (@string-contains? "foo" "foo") #t (list))
  (check-state (@string-contains? "foo" "of") #f (list)))

(define (check-string-contains?-symbolic)
  (check-state (@string-contains? x "foo") (@string-contains? x "foo") (list))
  (check-state (@string-contains? "foo" x) (@string-contains? "foo" x) (list))
  (check-state (@string-contains? x y) (@string-contains? x y) (list))
  (check-state (@string-contains? (merge a x #f) (merge b y #f)) (@string-contains? x y) (list a b)))

(define (check-string-prefix?-simplifications)
  (check-valid? (@string-prefix? x "") #t)
  (check-valid? (@string-prefix? "" x) (@equal? x ""))
  (check-valid? (@string-prefix? x x) #t))

(define (check-string-prefix?-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-prefix? 'a ""))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-prefix? "" xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-prefix? (merge a 2 #f) x))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-prefix? x (merge a xi #f))))))

(define (check-string-prefix?-lit)
  (check-state (@string-prefix? "foo" "f") #t (list))
  (check-state (@string-prefix? "foo" "o") #f (list))
  (check-state (@string-prefix? "foo" "fo") #t (list))
  (check-state (@string-prefix? "foo" "oo") #f (list))
  (check-state (@string-prefix? "foo" "foo") #t (list))
  (check-state (@string-prefix? "foo" "of") #f (list)))

(define (check-string-prefix?-sybmolic)
  (check-state (@string-prefix? x "foo") (@string-prefix? x "foo") (list))
  (check-state (@string-prefix? "foo" x) (@string-prefix? "foo" x) (list))
  (check-state (@string-prefix? x y) (@string-prefix? x y) (list))
  (check-state (@string-prefix? (merge a x #f) (merge b y #f)) (@string-prefix? x y) (list a b))
  (check-pred unsat? (solve (@string-prefix? x y) (! (@string-contains? x y))))
  (check-pred unsat? (solve (@string-prefix? x y) (@= (@string-length x) (@string-length y)) (! (@equal? x y))))
  (clear-asserts!))

(define (check-string-suffix?-simplifications)
  (check-valid? (@string-suffix? x "") #t)
  (check-valid? (@string-suffix? "" x) (@equal? x ""))
  (check-valid? (@string-suffix? x x) #t))

(define (check-string-suffix?-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-suffix? 'a ""))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-suffix? "" xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-suffix? (merge a 2 #f) x))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-suffix? x (merge a xi #f))))))

(define (check-string-suffix?-lit)
  (check-state (@string-suffix? "foo" "f") #f (list))
  (check-state (@string-suffix? "foo" "o") #t (list))
  (check-state (@string-suffix? "foo" "fo") #f (list))
  (check-state (@string-suffix? "foo" "oo") #t (list))
  (check-state (@string-suffix? "foo" "foo") #t (list))
  (check-state (@string-suffix? "foo" "of") #f (list)))

(define (check-string-suffix?-sybmolic)
  (check-state (@string-suffix? x "foo") (@string-suffix? x "foo") (list))
  (check-state (@string-suffix? "foo" x) (@string-suffix? "foo" x) (list))
  (check-state (@string-suffix? x y) (@string-suffix? x y) (list))
  (check-state (@string-suffix? (merge a x #f) (merge b y #f)) (@string-suffix? x y) (list a b))
  (check-pred unsat? (solve (@string-suffix? x y) (! (@string-contains? x y))))
  (check-pred unsat? (solve (@string-suffix? x y) (@= (@string-length x) (@string-length y)) (! (@equal? x y))))
  (clear-asserts!))

(define (check-string-replace-simplifications)
  (check-pred unsat? (solve (! (@equal? "" (@string-replace "" x y #:all? #f)))))
  (clear-asserts!)
  (check-pred unsat? (solve (! (@equal? (@string-append y x) (@string-replace x "" y #:all? #f)))))
  (clear-asserts!))

; Replace-all is not yet supported
(define (check-string-replace-all)
  (check-exn #px"replace all not supported" (thunk (with-asserts-only (@string-replace x y z))))
  (check-exn #px"replace all not supported" (thunk (with-asserts-only (@string-replace x y z #:all? #t))))
  (@string-replace x y z #:all? a)
  (define preconditions (asserts))
  (clear-asserts!)
  (check-pred unsat? (solve (@equal? x (@string-replace x y z #:all? a)) a (first preconditions)))
  (clear-asserts!))

(define (check-string-replace-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-replace 'a "" "" #:all? #f))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-replace "" xi "" #:all? #f))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-replace x y 2 #:all? #f))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-replace (merge a 2 #f) x y #:all? #f))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-replace x (merge a xi #f) y #:all? #f))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-replace x y (merge a 'a #f) #:all? #f)))))

(define (check-string-replace-lit)
  (check-state (@string-replace "foo" "f" "b" #:all? #f) "boo" (list))
  (check-state (@string-replace "foo" "fo" "y" #:all? #f) "yo" (list)))

(define (check-string-replace-symbolic)
  (check-state (@string-replace x y z #:all? a) (@string-replace x y z #:all? a) (list (@! a)))
  (clear-asserts!)
  (check-state (@string-replace "foo" "" x #:all? #f) (@string-replace "foo" "" x #:all? #f) (list))
  (check-state (@string-replace "foo" x y #:all? #f) (@string-replace "foo" x y #:all? #f) (list))
  (check-state (@string-replace "foo" x "" #:all? #f) (@string-replace "foo" x "" #:all? #f) (list))
  (check-state (@string-replace x "" y #:all? #f) (@string-replace x "" y #:all? #f) (list))
  (check-state (@string-replace x y "" #:all? #f) (@string-replace x y "" #:all? #f) (list))
  (check-state (@string-replace x y z #:all? #f) (@string-replace x y z #:all? #f) (list))
  (check-state (@string-replace (merge a x #f) (merge b y #f) (merge c z #f) #:all? #f) (@string-replace x y z #:all? #f) (list a b c))
  (clear-asserts!)
  (check-pred unsat? (solve (! (@equal? y (@string-replace x x y #:all? #f)))))
  (clear-asserts!))

; The behavior for this is more like Racket substring than Z3 string-at - we don't allow invalid indexes
(define (check-string-at-empty)
  (check-exn #px"index out of bounds" (thunk (with-asserts-only (@equal? x (@string-at "" xi)))))
  (clear-asserts!)
  (define y (@string-at x xi))
  (define preconditions (asserts))
  (clear-asserts!)
  (check-pred unsat? (solve (@equal? "" x) (first preconditions))))

(define (check-string-at-negative-offset)
  (@string-at x xi)
  (define preconditions (asserts))
  (clear-asserts!)
  (check-pred unsat? (solve (@<= xi -1) (first preconditions)))
  (clear-asserts!))

(define (check-string-at-out-of-bounds)
  (define y (@string-at x xi))
  (define preconditions (asserts))
  (clear-asserts!)
  (check-pred unsat? (solve (@>= xi (@string-length x)) (first preconditions)))
  (clear-asserts!))

(define (check-string-at-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-at 'a 0))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@string-at "foo" 1.2))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-at 0 xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-at (merge a xi #f) xi))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@string-at (merge a x #f) (merge b 1.2 #f))))))

(define (check-string-at-lit)
  (check-state (@string-at "bar" 0) "b" (list))
  (check-state (@string-at "bar" 1) "a" (list))
  (check-state (@string-at "bar" 2) "r" (list)))

(define (check-string-at-symbolic)
  (check-state (@string-at x 0) (@string-at x 0) (list (&& (@<= 1 (@string-length x)) (@< 0 (@string-length x)))))
  (clear-asserts!)
  (check-state (@string-at "foo" xi) (@string-at "foo" xi) (list (&& (@<= 0 xi) (@< xi 3))))
  (clear-asserts!)
  (check-state (@string-at x xi) (@string-at x xi) (list (&& (@<= 0 xi) (@< xi (@string-length x)) (@<= 1 (@string-length x)))))
  (clear-asserts!)
  (check-state (@string-at x xr) (@string-at x (@real->integer xr)) (list (&& (@<= 1 (@string-length x)) (@<= 0 (@real->integer xr)) (@< (@real->integer xr) (@string-length x))) (@int? xr)))
  (clear-asserts!)
  (check-state (@string-at (merge a x #f) (merge b xi #f)) (@string-at x xi) (list a b (&& (@<= 0 xi) (@< xi (@string-length x)) (@<= 1 (@string-length x)))))
  (clear-asserts!)
  (define xc (@string-at x xi))
  (define preconditions (asserts))
  (clear-asserts!)
  (check-pred unsat? (solve (! (@equal? xc (@substring x xi (@+ xi 1)))) (first preconditions)))
  (clear-asserts!))

(define (check-string-index-of-simplifications)
  (check-pred unsat? (solve (! (@equal? (@string-index-of x "" xi) 0))))
  (clear-asserts!)
  (define index (@string-index-of "" x xi))
  (check-pred unsat? (apply solve (&& (! (@equal? x "")) (@! index)) (asserts)))
  (clear-asserts!)
  (check-pred unsat? (solve (! (@equal? (@string-index-of x x) 0))))
  (clear-asserts!))

; Blocked by https://github.com/Z3Prover/z3/issues/875
;(define (check-string-index-of-not-contains)
  ;(define i (@string-index-of x y xi))
  ;(check-pred unsat? (apply solve (&& (@>= i 0) (! (@string-contains? x y))) (asserts)))
  ;(clear-asserts!))

(define (check-string-index-of-negative-offset)
  (define i (@string-index-of x y xi))
  (check-pred unsat? (apply solve (@>= (@string-index-of x y xi) 0) (@<= xi -1) (asserts)))
  (clear-asserts!))

(define (check-string-index-of-out-of-bounds)
  (define i (@string-index-of x y xi))
  (check-pred unsat? (solve (@>= i 0) (@>= xi (@string-length x)) (first (asserts))))
  (clear-asserts!))

(define (check-string-index-of-types)
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-index-of 'a "foo"))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-index-of "foo" 2))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@string-index-of "foo" "" 1.2))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-index-of "foo" xi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-index-of a x))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-index-of (merge a xi #f) x yi))))
  (check-exn #px"expected a string?" (thunk (with-asserts-only (@string-index-of (merge a x #f) (merge b xi #f) yi))))
  (check-exn #px"expected integer?" (thunk (with-asserts-only (@string-index-of (merge a x #f) (merge b y #f) (merge c 1.2 #f))))))

(define (check-string-index-of-no-offset)
  (check-state (@string-index-of "foo" "f") 0 (list))
  (check-state (@string-index-of "foo" "o") 1 (list))
  (check-state (@string-index-of "foo" "fo") 0 (list))
  (check-state (@string-index-of "foo" "oo") 1 (list))
  (check-state (@string-index-of "foo" "foo") 0 (list))
  (check-state (@string-index-of "foo" "of") #f (list)))

(define (check-string-index-of-offset)
  (check-state (@string-index-of "foo" "o" 2) 2 (list))
  (check-state (@string-index-of "foo" "foo" 1) #f (list)))

(define (check-string-index-of-symbolic)
  (check-state (@string-index-of x "foo" 0) (@string-index-of x "foo" 0) (list (@< 0 (@string-length x))))
  (clear-asserts!)
  (check-state (@string-index-of x y 0) (@string-index-of x y 0) (list (@< 0 (@string-length x))))
  (clear-asserts!)
  (check-state (@string-index-of x "foo" xi) (@string-index-of x "foo" xi) (list (@<= 0 xi) (@< xi (@string-length x))))
  (clear-asserts!)
  (check-state (@string-index-of "foo" x 0) (@string-index-of "foo" x 0) (list))
  (check-state (@string-index-of "foo" "bar" xi) (@string-index-of "foo" "bar" xi) (list (@<= 0 xi) (@< xi 3)))
  (clear-asserts!)
  (check-state (@string-index-of "foo" x xi) (@string-index-of "foo" x xi) (list (@<= 0 xi) (@< xi 3)))
  (clear-asserts!)
  (check-state (@string-index-of x y xi) (@string-index-of x y xi) (list (@<= 0 xi) (@< xi (@string-length x))))
  (clear-asserts!)
  (check-state (@string-index-of x y) (@string-index-of x y) (list (@< 0 (@string-length x))))
  (clear-asserts!)
  (check-state (@string-index-of x y xr) (@string-index-of x y (@real->integer xr)) (list (@int? xr) (@<= 0 (@real->integer xr)) (@< (@real->integer xr) (@string-length x))))
  (clear-asserts!)
  (check-state (@string-index-of (merge a x #f) (merge b y #f)) (@string-index-of x y) (list (@< 0 (@string-length x)) a b))
  (clear-asserts!)
  (check-state (@string-index-of (merge a x #f) (merge b y #f) (merge c xi #f)) (@string-index-of x y xi) (list (@<= 0 xi) (@< xi (@string-length x)) a b c)))

(define tests:string?
  (test-suite+
   "Tests for string? in rosette/base/string.rkt"
   (check-string?)
   (check-string-cast)
   (check-=-simplifications)))

(define tests:string-append
  (test-suite+
   "Tests for string-append in rosette/base/string.rkt"
   (check-string-append-no-args)
   (check-string-append-simplifications)
   (check-string-append-types)
   (check-string-append-lit)
   (check-string-append-symbolic))) 

(define tests:string-length
  (test-suite+
   "Tests for string-length in rosette/base/string.rkt"
   (check-string-length-empty)
   (check-string-length-types)
   (check-string-length-lit)
   (check-string-length-symbolic))) 

(define tests:string->integer
  (test-suite+
   "Tests for string->integer in rosette/base/string.rkt"
   (check-string->integer-simplifications)
   (check-string->integer-empty)
   (check-string->integer-non-number)
   (check-string->integer-non-integer)
   (check-string->integer-types)
   (check-string->integer-lit)
   (check-string->integer-symbolic)))

(define tests:integer->string
  (test-suite+
   "Tests for integer->string in rosette/base/string.rkt"
   (check-integer->string-simplifications)
   (check-integer->string-types)
   (check-integer->string-lit)
   (check-integer->string-symbolic)))

(define tests:substring
  (test-suite+
   "Tests for substring in rosette/base/string.rkt"
   (check-substring-empty)
   (check-substring-simplifications)
   (check-substring-negative-i)
   (check-substring-out-of-bounds)
   (check-substring-no-j)
   (check-substring-types)
   (check-substring-lit)
   (check-substring-symbolic)))

(define tests:string-contains?
  (test-suite+
   "Tests for string-contains? in rosette/base/string.rkt"
   (check-string-contains?-simplifications)
   (check-string-contains?-types)
   (check-string-contains?-lit)
   (check-string-contains?-symbolic)))

(define tests:string-prefix?
  (test-suite+
   "Tests for string-prefix? in rosette/base/string.rkt"
   (check-string-prefix?-simplifications)
   (check-string-prefix?-types)
   (check-string-prefix?-lit)
   (check-string-prefix?-sybmolic))) 

(define tests:string-suffix?
  (test-suite+
   "Tests for string-suffix? in rosette/base/string.rkt"
   (check-string-suffix?-simplifications)
   (check-string-suffix?-types)
   (check-string-suffix?-lit)
   (check-string-suffix?-sybmolic)))

(define tests:string-replace
  (test-suite+
   "Tests for string-replace in rosette/base/string.rkt"
   (check-string-replace-simplifications)
   (check-string-replace-all)
   (check-string-replace-types)
   (check-string-replace-lit)
   (check-string-replace-symbolic)))

(define tests:string-at
  (test-suite+
   "Tests for string-at in rosette/base/string.rkt"
   (check-string-at-empty)
   (check-string-at-negative-offset)
   (check-string-at-out-of-bounds)
   (check-string-at-types)
   (check-string-at-lit)
   (check-string-at-symbolic))) 

(define tests:string-index-of
  (test-suite+
   "Tests for string-index-of in rosette/base/string.rkt"
   (check-string-index-of-simplifications)
   ;(check-string-index-of-not-contains) Blocked by https://github.com/Z3Prover/z3/issues/875
   (check-string-index-of-negative-offset)
   (check-string-index-of-out-of-bounds)
   (check-string-index-of-types)
   (check-string-index-of-no-offset)
   (check-string-index-of-offset)
   (check-string-index-of-symbolic)))

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

(solver-shutdown (solver))