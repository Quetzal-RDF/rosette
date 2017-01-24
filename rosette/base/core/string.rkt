#lang racket

; Some of the preliminary work on strings is taken from this pull request:
; https://github.com/emina/rosette/pull/7
;
; It has been adapted for Rosette 2.0 and extended with support for new operations. 

(require racket/splicing
         "term.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" "real.rkt"
         "safe.rkt" "../adt/seq.rkt")

(provide @string? @string/equal? @string-append @string-length @substring
         @string-contains? @string-prefix? @string-suffix?
         @string-replace @string-replace-internal
         @string->integer @integer->string
         @string-at @string-index-of @index-of
         @string-set! @string-fill!
         @string-copy! T*->string?
         lift-op-generic)

(define-lifted-type @string?
  #:base string?
  #:is-a? (instance-of? string? @string?)
  #:methods
  [(define (solvable-default self) "")
   (define (type-eq? self u v) (@string/equal? u v)) 
   (define (type-equal? self u v) (@string/equal? u v))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? string?) v]
       [(term _ (== self)) v]
       [(union (list _ ... (cons g (and (app type-of (== @string?)) u)) _ ...))
        (assert g (thunk (raise-argument-error caller "expected a string?" v)))
        u]
       [_ (assert #f (thunk (raise-argument-error caller "expected a string?" v)))])) 
   (define (type-compress self force? ps) (string/compress force? ps))])     

; The value of the force? parameter is ignored since 
; we treat all strings as immutable and therefore always
; compressible (forced) into a term.
(define (string/compress force? ps)
  (match ps
    [(list _) ps]
    [(list (cons g a) (cons (expression (== !) g) b)) (list (cons #t (ite g a b)))]
    [(list (cons (expression (== !) g) b) (cons g a)) (list (cons #t (ite g a b)))]
    [(list (cons g a) ...)
     (list (cons (apply || g)
                 (apply @string-append (for/list ([guard g][str a]) (ite guard str "")))))]))

;; ----------------- Lifting utilities ----------------- ;;

(define (safe-apply-n op xs @ts? @default?)
  (define caller (object-name op)) 
  (cond
    [(empty? @ts?) (apply op (for/list ([x xs]) (type-cast @default? x caller)))]
    [else (apply op (for/list ([x xs] [@t? @ts?]) (type-cast @t? x caller)))]))

(define (safe-apply-1 op x @ts? @default?)
  (safe-apply-n op (list x) @ts? @default?))

(define (safe-apply-2 op x y @ts? @default?)
  (safe-apply-n op (list x y) @ts? @default?))

(define (lift-op-generic op @ts? @default?)
  (case (procedure-arity op)
    [(1)  (lambda (x) (safe-apply-1 op x @ts? @default?))]
    [(2)  (lambda (x y) (safe-apply-2 op x y @ts? @default?))]
    [else
     (case-lambda
       [() (op)]
       [(x) (safe-apply-1 op x @ts? @default?)]
       [(x y) (safe-apply-2 op x y @ts? @default?)]
       [xs (safe-apply-n op xs @ts? @default?)])]))

(define (lift-op op . @ts?)
  (lift-op-generic op @ts? @string?))

(define T*->string? (const @string?))

;; ----------------- String Operators ----------------- ;;

(define ($= x y)
  (match* (x y)
    [((? string?) (? string?)) (equal? x y)]
    [(_ (== x)) #t]
    [((expression (== ite) a (? string? b) (? string? c)) (? string? d))
     (|| (&& a (equal? b d)) (&& (! a) (equal? c d)))]
    [((? string? d) (expression (== ite) a (? string? b) (? string? c)))
     (|| (&& a (equal? b d)) (&& (! a) (equal? c d)))]
    [((expression (== ite) a (? string? b) (? string? c)) 
      (expression (== ite) d (? string? e) (? string? f)))
     (let ([b~e (equal? b e)] 
           [b~f (equal? b f)] 
           [c~e (equal? c e)] 
           [c~f (equal? c f)])
       (or (and b~e b~f c~e c~f)
           (|| (&& a d b~e) (&& a (! d) b~f) (&& (! a) d c~e) (&& (! a) (! d) c~f))))]
    [(a (expression (== @string-append) (? string? s) a))
     (equal? "" s)]
    [((expression (== @string-append) (? string? s) a) a) (equal? s "")]
    [(_ _) (sort/expression @string/equal? x y)]))

(define-operator @string/equal?
  #:identifier '=
  #:range T*->boolean?
  #:unsafe $=
  #:safe (lift-op $=))

; string-append
; Simplifications for two values
(define (simplify-string-append-2 x y)
  (match* (x y)
    [((? string?) (? string?)) (string-append x y)]
    [(_ "") x]
    [("" _) y]
    [((expression (== ite) a (? string? b) (? string? c)) (? string?))
     (ite a (string-append b y) (string-append c y))]
    [(_ _) (expression @string-append x y)]))

; Simplifications for more values
(define (string-append-simplify xs)
  (match xs
    [(list) xs]
    [(list _) xs]
    [(list x y) (list (simplify-string-append-2 x y))]
    [(list "" rest ...) (string-append-simplify rest)]
    [(list-rest (? string? x) ..2 rest)
     (list* (apply string-append x) (string-append-simplify rest))]
    [(list x rest ...)
     (list* x (string-append-simplify rest))]))

(define ($string-append . xs)
  (match xs
    [`() ""]
    [(list x) x]
    [(list x y) (simplify-string-append-2 x y)]
    [_
     (match (string-append-simplify xs)
       [(list x) x]
       [ys (apply expression @string-append ys)])]))

(define-operator @string-append
  #:identifier 'string-append
  #:range T*->T
  #:unsafe $string-append
  #:safe (lift-op $string-append))

; string-length
(define ($string-length s)
  (match s
    [(? string? x) (string-length x)]
    [x (expression @string-length x)]))

(define-operator @string-length
  #:identifier 'string-length
  #:range T*->integer?
  #:unsafe $string-length
  #:safe (lift-op $string-length))

; integer->string
(define (simplify-integer->string i)
  (match i
    [(expression (== @string->integer) s)
     (if (@= (@string->integer s) 0) "0" s)]
    [_ (expression @integer->string i)]))

(define (integer->string i)
  (match i
    [(? integer?) (number->string i)]
    [_ (simplify-integer->string i)]))
  
(define-operator @integer->string
  #:identifier 'integer->string
  #:range T*->string?
  #:unsafe integer->string
  #:safe (lift-op integer->string @integer?))

; string->integer
(define (simplify-string->integer s)
  (match s
    [(expression (== @integer->string) (? @integer? i)) i]
    [_ (expression @string->integer s)]))

(define (string->integer s)
  (match s
    [(? string?)
     (let ((n (string->number s)))
       (if (integer? n) n 0))] 
    [_ (simplify-string->integer s)]))

(define-operator @string->integer
  #:identifier 'string->integer
  #:range T*->integer?
  #:unsafe string->integer
  #:safe (lift-op string->integer))

; substring
(define (simplify-substring s i j)
  (match* (s i j)
    [(_ 0 0) ""]
    [(_ 0 (expression (== @string-length) s)) s]
    [(_ _ _) (expression @substring s i j)]))

(define ($substring s i [j (@string-length s)])
  (if (and (string? s) (number? i) (number? j)) 
      (substring s i j)
      (simplify-substring s i j)))

(define (guarded-substring s i [j (@string-length s)])
  (assert (&& (@>= i 0) (@<= i j) (@<= j (@string-length s)))
          (thunk (error 'substring "index out of bounds")))
  ($substring s i j))
  
(define-operator @substring 
  #:identifier 'substring
  #:range T*->string?
  #:unsafe $substring
  #:safe (lift-op guarded-substring @string? @integer? @integer?))

; string-contains?
(define (simplify-string-contains? s p)
  (match* (s p)
    [(_ "") #t]
    [("" x) (@string/equal? x "")]
    [(x (expression (== @substring) x (? @integer? i) (? @integer? j))) #t] 
    [(_ _) (expression @string-contains? s p)]))

(define ($string-contains? s p)
  (if (and (string? s) (string? p))
      (string-contains? s p)
      (simplify-string-contains? s p)))

(define-operator @string-contains?
  #:identifier 'string-contains?
  #:range T*->boolean? 
  #:unsafe $string-contains?
  #:safe (lift-op $string-contains?))

; string-replace
; Note that in Racket, string-replace x "" y = (string-append y x) for all y, x
; SMTLib standards also dictate this (see https://github.com/Z3Prover/z3/issues/703)
(define (simplify-string-replace s from to all?)
  (match* (s from to)
    [(x "" y) (@string-append y x)]
    [("" _ _) ""]
    [(_ (== s) y) y]
    [(_ _ _) (expression @string-replace-internal s from to all?)]))

; String-replace matches Racket semantics, but doesn't support all? #t or from of type regexp? yet
(define ($string-replace s from to [all? #t])
  (if (and (string? s) (string? from) (string? to))
      (string-replace s from to #:all? all?)
      (simplify-string-replace s from to all?)))

(define-operator @string-replace-internal
  #:identifier 'string-replace
  #:range T*->string?
  #:unsafe $string-replace
  #:safe
  (lambda (s from to [all? #t])
    (define caller 'string-replace)
    (assert (@! all?) (thunk (error caller "replace all not supported, use all? #f instead")))
    ($string-replace
     (type-cast @string? s caller)
     (type-cast @string? from caller) 
     (type-cast @string? to caller)
     all?)))

(define (@string-replace s from to #:all? [all? #t])
  (@string-replace-internal s from to all?))

; string-prefix?
(define (simplify-string-prefix? s pre)
  (match* (s pre)
    [(_ "") #t]
    [("" x) (@string/equal? x "")]
    [(_ (== s)) #t]
    [(_ _) (expression @string-prefix? s pre)]))

(define ($string-prefix? s pre)
  (match* (s pre)
    [((? string?) (? string?)) (string-prefix? s pre)]
    [(_ _) (simplify-string-prefix? s pre)]))
 
(define-operator @string-prefix?
  #:identifier 'string-prefix?
  #:range T*->boolean? 
  #:unsafe $string-prefix?
  #:safe (lift-op $string-prefix?))

; string-suffix?
(define (simplify-string-suffix? s suf)
  (match* (s suf)
    [(_ "") #t]
    [("" x) (@string/equal? x "")]
    [(_ (== s)) #t]
    [(_ _) (expression @string-suffix? s suf)]))

(define ($string-suffix? s suf)
  (if (and (string? s) (string? suf))
      (string-suffix? s suf)
      (simplify-string-suffix? s suf)))
              
(define-operator @string-suffix?
  #:identifier 'string-suffix?
  #:range T*->boolean?
  #:unsafe $string-suffix?
  #:safe (lift-op $string-suffix?))

; string-at
(define (string-at s offset)
  (if (and (string? s) (number? offset))
      (@substring s offset (+ offset 1))
      (expression @string-at s offset)))

(define (guarded-string-at s offset)
  (assert (&& (@<= 0 offset) (@< offset (@string-length s)) (@<= 1 (@string-length s)))
          (thunk (error 'string-at "index out of bounds")))
  (string-at s offset))

(define-operator @string-at
  #:identifier 'string-at
  #:range T*->string?
  #:unsafe string-at
  #:safe (lift-op guarded-string-at @string? @integer?))

; string-index-of with Racket-like semantics even though
; this is not a Racket function;
; missing indexes return #f instead of -1
(define (simplify-string-index-of s sub offset)
  (match* (s sub offset)
    [(_ "" _) 0]
    [("" _ _) #f]
    [(_ (== s) 0) 0]  
    [(_ _ _)
     (let ([index (@index-of s sub offset)])
       (if (@>= index 0)
           index
           #f))]))

(define ($string-index-of s sub [offset 0])
  (match* (s sub offset)
    [((? string?) (? string?) (? number?)) (index-of s sub offset)]
    [(_ _ _) (simplify-string-index-of s sub offset)]))

(define (guarded-string-index-of s sub [offset 0])
  (assert (@<= 0 offset) (thunk (error 'string-index-of "negative offset" offset)))
  (assert (@< offset (@string-length s)) (thunk (error 'string-index-of "offset out of bounds" offset)))
  ($string-index-of s sub offset))

(define-operator @string-index-of
  #:identifier 'string-index-of
  #:range T*->integer?
  #:unsafe $string-index-of
  #:safe (lift-op guarded-string-index-of @string? @string? @integer?))

; string-index-of internal representation
(define (index-of s sub offset)
  (if (@string-contains? s sub)
     (let ([len-sub (@string-length sub)] [len (@string-length s)])
       (for/or ([i (in-range offset (+ (- len len-sub) 1))])
         (if (@string-contains? (@substring s i (+ i len-sub)) sub)
             i
             #f)))
     #f))

(define ($index-of s sub [offset 0])
  (if (and (string? s) (string? sub) (number? offset))
      (let ([index (index-of s sub offset)])
        (if index index -1))
      (expression @index-of s sub offset)))

(define-operator @index-of
  #:identifier 'index-of
  #:range T*->integer?
  #:unsafe $index-of
  #:safe (lift-op $index-of @string? @string? @integer?))

; We are going to disable all mutation operations on strings.

(define disable-mutation (lambda xs (error 'string-set! "string mutation not supported")))

(define @string-set! (impersonate-procedure string-set! disable-mutation))    
(define @string-fill! (impersonate-procedure string-fill! disable-mutation))
(define @string-copy! (impersonate-procedure string-copy! disable-mutation))
