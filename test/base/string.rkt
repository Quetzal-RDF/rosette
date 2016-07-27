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

(define (check-string-length-empty)
  (check-state (@string-length "") 0 (list)))

(define (check-string-length-types)
  (check-exn #px"expected string?" (@string-length 'a))
  (check-exn #px"expected string?" (@string-length xi))
  (check-exn #px"expected string?" (@string-length (merge a 2 #f))))

(define (check-string-length-lit)
  (check-state (@string-length "a") 1 (list))
  (check-state (@string-length "foo") 3 (list)))

(define (check-string-length-symbolic)
  (check-state (@string-length x) (@string-length x) (list))
  (check-state (@string-length (merge a x #f)) (@string-length x) (list a)))

(define (check-string->integer-empty)
  (check-state (@string->integer "") #f (list)))
  
(define (check-string->integer-non-number)
  (check-state (@string->integer "foo") #f (list))
  (check-state (@string->integer "foo1") #f (list))
  (check-pred unsat? (solve (@= x (@string->append "a" y)) (@= xi (@string->integer x))))
  (clear-asserts!))

(define (check-string->integer-non-integer)
  (check-state (@string->integer "1.2") #f (list)))

; TODO refactor these check-exns
(define (check-string->integer-types)
  (check-exn #px"expected string?" (@string->integer 'a))
  (check-exn #px"expected string?" (@string->integer xi))
  (check-exn #px"expected string?" (@string->integer (merge a 2 #f))))

(define (check-string->integer-lit)
  (check-state (@string->integer "1") 1 (list))
  (check-state (@string->integer "123") 123 (list)))

(define (check-string->integer-symbolic)
  (check-state (@string->integer x) (@string->integer x) (list))
  (check-state (@string->integer (merge a x #f)) (@string->integer x) (list a))
  (check-valid? (@string->integer (@integer->string xi)) xi))

(define (check-integer->string-types)
  (check-exn #px"expected integer?" (@integer->string 'a))
  (check-exn #px"expected integer?" (@integer->string 1.2))
  (check-exn #px"expected integer?" (@integer->string x))
  (check-exn #px"expected integer?" (@integer->string xr))
  (check-exn #px"expected integer?" (@integer->string (merge a "" #f)))
  (check-exn #px"expected integer?" (@integer->string (merge a xr #f))))

(define (check-integer->string-lit)
  (check-state (@integer->string 0) "0" (list))
  (check-state (@integer->string 123) "123" (list)))

(define (check-integer->string-symbolic)
  (check-state (@integer->string xi) (@integer->string xi) (list))
  (check-state (@integer->string (merge a xi #f)) (@integer->string xi) (list a)))

(define (check-substring-empty)
  (check-state (@substring "" 0) "" (list))
  (check-valid? (@substring x 0 0) ""))

(define (check-substring-negative-i)
  (check-pred unsat? (solve (@substring x xi) (@<= xi -1)))
  (check-pred unsat? (solve (@substring x xi yi) (@<= xi -1)))
  (clear-asserts!))

(define (check-substring-out-of-bounds)
  (check-pred unsat? (solve (@substring x xi yi) (@> xi yi)))
  (check-pred unsat? (solve (@substring x xi yi) (@>= yi (@string-length x))))
  (clear-asserts!))

(define (check-substring-no-j)
  (check-valid? (@substring x xi) (@substring x xi (@string-length x))))

(define (check-substring-types)
  (check-exn #px"expected string?" (@substring 'a))
  (check-exn #px"expected string?" (@substring xi))
  (check-exn #px"expected string?" (@substring (merge a 2 #f)))
  (check-exn #px"expected integer?" (@substring x y))
  (check-exn #px"expected integer?" (@substring x xi y)))

(define (check-substring-lit)
  (check-state (@substring "foo" 0) "foo" (list))
  (check-state (@substring "foo" 0 1) "f" (list))
  (check-state (@substring "foo" 0 2) "fo" (list))
  (check-state (@substring "foo" 0 3) "foo" (list)))

(define (check-substring-symbolic)
  (check-state (@substring "foo" xi) (@substring "foo" xi) (list))
  (check-state (@substring "foo" 0 xi) (@substring "foo" 0 xi) (list))
  (check-state (@substring "foo" xi yi) (@substring "foo" xi yi) (list))
  (check-state (@substring x 0) (@substring x 0) (list))
  (check-state (@substring x 0 1) (@substring x 0 1) (list))
  (check-state (@substring x xi) (@substring x xi) (list))
  (check-state (@substring x xi 1) (@substring x xi 1) (list))
  (check-state (@substring x 0 xi) (@substring x 0 xi) (list))
  (check-state (@substring (merge a x #f) (merge b xi #f) (merge c yi #f)) (@substring x xi yi) (list a b c))
  (check-valid? (@substring x 0) x))

(define (check-string-contains?-empty)
  (check-valid? (@string-contains? x "") #t)
  (check-pred unsat? (solve (@string-contains? "" x) (! (@= x ""))))
  (clear-asserts!))

(define (check-string-contains?-types)
  (check-exn #px"expected string?" (@string-contains? 'a ""))
  (check-exn #px"expected string?" (@string-contains? "" xi))
  (check-exn #px"expected string?" (@string-contains? (merge a 2 #f) x))
  (check-exn #px"expected string?" (@string-contains? x (merge a xi #f))))

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
  (check-state (@string-contains? (merge a x #f) (merge b y #f)) (@string-contains? x y) (list a b))
  (check-valid (@string-contains? x (@substring x xi yi)) #t))

(define (check-string-prefix?-empty)
  (check-valid (@string-prefix? x "") #t)
  (check-pred unsat? (solve (@string-prefix? "" x) (! (@= x ""))))
  (clear-asserts!))

(define (check-string-prefix?-types)
  (check-exn #px"expected string?" (@string-prefix? 'a ""))
  (check-exn #px"expected string?" (@string-prefix? "" xi))
  (check-exn #px"expected string?" (@string-prefix? (merge a 2 #f) x))
  (check-exn #px"expected string?" (@string-prefix? x (merge a xi #f))))

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
  (check-pred unsat? (solve (@string-prefix? x y) (@= (@string-length x) (@string-length y)) (! (@= x y))))
  (clear-asserts!)
  (check-valid? (@string-prefix? x x) #t))

(define (check-string-suffix?-empty)
  (check-valid (@string-suffix? x "") #t)
  (check-pred unsat? (solve (@string-suffix? "" x) (! (@= x ""))))
  (clear-asserts!))

(define (check-string-suffix?-types)
  (check-exn #px"expected string?" (@string-suffix? 'a ""))
  (check-exn #px"expected string?" (@string-suffix? "" xi))
  (check-exn #px"expected string?" (@string-suffix? (merge a 2 #f) x))
  (check-exn #px"expected string?" (@string-suffix? x (merge a xi #f))))

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
  (check-pred unsat? (solve (@string-suffix? x y) (@= (@string-length x) (@string-length y)) (! (@= x y))))
  (clear-asserts!)
  (check-valid? (@string-suffix? x x) #t))

(define (check-string-replace-empty)
  (check-valid? (@string-replace "" x y #:all? #f) "")
  (check-valid? (@string-replace x "" y #:all? #f) (@string-append y x)))

; Replace-all is not yet supported
(define (check-string-replace-all) 
  (check-pred unsat? (solve (@string-replace x y z)))
  (check-pred unsat? (solve (@string-replace x y z #:all? #t)))
  (check-pred unsat? (solve (@string-replace x y z #:all? a) a))
  (clear-asserts!))

(define (check-string-replace-types)
  (check-exn #px"expected string?" (@string-replace 'a "" "" #:all? #f))
  (check-exn #px"expected string?" (@string-replace "" xi "" #:all? #f))
  (check-exn #px"expected string?" (@string-replace x y 2 #:all? #f))
  (check-exn #px"expected string?" (@string-replace (merge a 2 #f) x y #:all? #f))
  (check-exn #px"expected string?" (@string-replace x (merge a xi #f) y #:all? #f))
  (check-exn #px"expected string?" (@string-replace x y (merge 'a xi #f) #:all? #f)))

(define (check-string-replace-lit)
  (check-state (@string-replace "foo" "f" "b" #:all? #f) "boo" (list))
  (check-state (@string-replace "foo" "fo" "y" #:all? #f) "yo" (list)))

(define (check-string-replace-symbolic)
  (check-state (@string-replace "foo" "" x #:all? #f) (@string-replace "foo" "" x #:all? #f) (list))
  (check-state (@string-replace "foo" x y #:all? #f) (@string-replace "foo" x y #:all? #f) (list))
  (check-state (@string-replace "foo" x "" #:all? #f) (@string-replace "foo" x "" #:all? #f) (list))
  (check-state (@string-replace x "" y #:all? #f) (@string-replace x "" y #:all? #f) (list))
  (check-state (@string-replace x y "" #:all? #f) (@string-replace x y "" #:all? #f) (list))
  (check-state (@string-replace x y z #:all? #f) (@string-replace x y z #:all? #f) (list))
  (check-state (@string-replace (merge a x #f) (merge b y #f) (merge c z #f)) (@string-replace x y z) (list a b c))
  (check-valid? (@string-replace x x y #:all? #f) y))

; The behavior for this is more like Racket substring than Z3 string-at - we don't allow invalid indexes
(define (check-string-at-empty)
  (check-pred unsat? (solve (@string-at "" xi)))
  (clear-asserts!))

(define (check-string-at-negative-offset)
  (check-pred unsat? (solve (@string-at x xi) (@<= xi -1)))
  (clear-asserts!))

(define (check-string-at-out-of-bounds)
  (check-pred unsat? (solve (@sring-at x xi) (@>= xi (@string-length x))))
  (clear-asserts!))

(define (check-string-at-types)
  (check-exn #px"expected string?" (@string-at 'a 0))
  (check-exn #px"expected integer?" (@string-at "foo" 1.2))
  (check-exn #px"expected string?" (@string-at 0 xi))
  (check-exn #px"expected integer?" (@string-at x xr))
  (check-exn #px"expected string?" (@string-at (merge a xi #f) xi))
  (check-exn #px"expected integer?" (@string-at (merge a x #f) (merge b xr #f))))

(define (check-string-at-lit)
  (check-state (@string-at "bar" 0) "b" (list))
  (check-state (@string-at "bar" 1) "a" (list))
  (check-state (@string-at "bar" 2) "r" (list)))

(define (check-string-at-symbolic)
  (check-state (@string-at x 0) (@string-at x 0) (list))
  (check-state (@string-at "" xi) (@string-at "" xi) (list))
  (check-state (@string-at x xi) (@string-at x xi) (list))
  (check-state (@string-at (merge a x #f) (merge b xi #f)) (@string-at x xi) (list a b))
  (check-valid? (@string-at x xi) (@substring x xi (@+ xi 1)))) 

; string-index-of s sub [offset 0]
(define (check-string-index-of-empty)
  (check-valid? (@string-index-of x "" xi) 0)
  (check-pred unsat? (solve (@>= (@string-index-of "" x xi) 0) (! (@= x ""))))
  (clear-asserts!))

(define (check-string-index-of-not-contains)
  (check-pred unsat? (solve (@>= (@string-index-of x y xi) 0) (! (@string-contains? x y))))
  (clear-asserts!))

(define (check-string-index-of-negative-offset)
  (check-pred unsat? (solve (@>= (@string-index-of x y xi) 0) (@<= xi -1)))
  (clear-asserts!))

(define (check-string-index-of-out-of-bounds)
  (check-pred unsat? (solve (@>= (@string-index-of x y xi) 0) (@>= xi (@string-length x))))
  (clear-asserts!))

(define (check-string-index-of-types)
  (check-exn #px"expected string?" (@string-index-of 'a "foo"))
  (check-exn #px"expected string?" (@string-index-of "foo" 2))
  (check-exn #px"expected integer?" (@string-index-of "foo" "" 1.2))
  (check-exn #px"expected string?" (@string-index-of "foo" xi))
  (check-exn #px"expected string?" (@string-index-of a x))
  (check-exn #px"expected integer?" (@string-index-of x y xr))
  (check-exn #px"expected string?" (@string-index-of (merge a xi #f) x yi))
  (check-exn #px"expected string?" (@string-index-of (merge a x #f) (merge b xi #f) yi))
  (check-exn #px"expected integer?" (@string-idex-of (merge a x #f) (merge b y #f) (merge c xr #f))))

(define (check-string-index-of-no-offset)
  (check-state (@string-index-of "foo" "f") 0 (list))
  (check-state (@string-index-of "foo" "o") 1 (list))
  (check-state (@string-index-of "foo" "fo") 0 (list))
  (check-state (@string-index-of "foo" "oo") 1 (list))
  (check-state (@string-index-of "foo" "foo") 0 (list))
  (check-state (@string-index-of "foo" "of") -1 (list)))

(define (check-string-index-of-offset)
  (check-state (@string-index-of "foo" "o" 2) 2 (list))
  (check-state (@string-index-of "foo" "foo" 1) -1 (list)))

(define (check-string-index-of-symbolic)
  (check-state (@string-index-of x "foo" 0) (@string-index-of x "foo" 0) (list))
  (check-state (@string-index-of x y 0) (@string-index-of x y 0) (list))
  (check-state (@string-index-of x "foo" xi) (@string-index-of x "foo" xi) (list))
  (check-state (@string-index-of "foo" x 0) (@string-index-of "foo" x 0) (list))
  (check-state (@string-index-of "foo" "bar" xi) (@string-index-of "foo" "bar" xi) (list))
  (check-state (@string-index-of "foo" x xi) (@string-index-of "foo" x xi) (list))
  (check-state (@string-index-of x y xi) (@string-index-of x y xi) (list))
  (check-state (@string-index-of x y) (@string-index-of x y) (list))
  (check-state (@string-index-of (merge a x #f) (merge b y #f)) (@string-index-of x y) (list a b))
  (check-state (@string-index-of (merge a x #f) (merge b y #f) (merge c xi #f)) (@string-index-of x y xi) (list a b c)) 
  (check-valid? (@string-index-of x x) 0))

(define tests:string?
  (test-suite+
   "Tests for string? in rosette/base/string.rkt"
   (check-string?)
   (check-string-cast)))

; TODO do I need to test string/equal? string/compress? etc

(define tests:string-append
  (test-suite+
   "Tests for string-append in rosette/base/string.rkt"
   (check-string-append-no-args)
   (check-string-append-empty)
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
   (check-string->integer-empty)
   (check-string->integer-non-number)
   (check-string->integer-non-integer)
   (check-string->integer-types)
   (check-string->integer-lit)
   (check-string->integer-symbolic)))

(define tests:integer->string
  (test-suite+
   "Tests for integer->string in rosette/base/string.rkt"
   (check-integer->string-types)
   (check-integer->string-lit)
   (check-integer->string-symbolic))) 

(define tests:substring
  (test-suite+
   "Tests for substring in rosette/base/string.rkt"
   (check-substring-empty)
   (check-substring-negative-i)
   (check-substring-out-of-bounds)
   (check-substring-no-j)
   (check-substring-types)
   (check-substring-lit)
   (check-substring-symbolic)))

(define tests:string-contains?
  (test-suite+
   "Tests for string-contains? in rosette/base/string.rkt"
   (check-string-contains?-empty)
   (check-string-contains?-types)
   (check-string-contains?-lit)
   (check-string-contains?-symbolic)))

(define tests:string-prefix?
  (test-suite+
   "Tests for string-prefix? in rosette/base/string.rkt"
   (check-string-prefix?-empty)
   (check-string-prefix?-types)
   (check-string-prefix?-lit)
   (check-string-prefix?-sybmolic))) 

(define tests:string-suffix?
  (test-suite+
   "Tests for string-suffix? in rosette/base/string.rkt"
   (check-string-suffix?-empty)
   (check-string-suffix?-types)
   (check-string-suffix?-lit)
   (check-string-suffix?-sybmolic)))

(define tests:string-replace
  (test-suite+
   "Tests for string-replace in rosette/base/string.rkt"
   (check-string-replace-empty)
   (check-string-replace-all)
   (check-string-replace-first)
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
   (check-string-index-of-empty)
   (check-string-index-of-not-contains)
   (check-string-index-of-negative-offset)
   (check-string-index-of-out-of-bounds)
   (check-string-index-of-types)
   (check-string-index-of-no-offset)
   (check-string-index-of-offset)
   (check-string-index-of-symbolic)))

; TODO test that strings are immutable

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