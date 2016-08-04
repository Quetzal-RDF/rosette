#lang racket

(require racket/splicing
         "term.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" "real.rkt"
         "safe.rkt" "../adt/seq.rkt")

(provide @string? @string-append @string-length @substring
         @string-contains? @string-prefix? @string-suffix?
         @string-replace @string-replace-internal
         @string->integer @integer->string
         @string-at @string-index-of @string-set! @string-fill!
         @string-copy! T*->string?)

(define (string/equal? x y)
  (match* (x y)
    [((? string?) (? string?)) (equal? x y)]
    [(_ _) (=? x y)]))

(define-lifted-type @string?
  #:base string?
  #:is-a? (instance-of? string? @string?)
  #:methods
  [(define (solvable-default self) "")
   (define (type-eq? self u v) (string/equal? u v)) 
   (define (type-equal? self u v) (string/equal? u v))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? string?) v]
       [(term _ (== self)) v]
       [(union (list _ ... (cons g (and (app type-of (== @string?)) u)) _ ...))
        (assert g (thunk (raise-argument-error caller "expected a string?" v)))
        u]
       [_ (assert #f (thunk (raise-argument-error caller "expected a string?" v)))])) 
   (define (type-compress self force? ps) string/compress)])     

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

; TODO are safe-apply-1 and safe-apply-2 usually called out for the sake of efficiency?

(define (safe-apply-n op xs @ts?)
  (define caller (object-name op)) 
  (cond
    [(empty? @ts?) (apply op (for/list ([x xs]) (type-cast @string? x caller)))]
    [else (apply op (for/list ([x xs] [@t? @ts?]) (type-cast @t? x caller)))]))

(define (safe-apply-1 op x @ts?)
  (safe-apply-n op (list x) @ts?))

(define (safe-apply-2 op x y @ts?)
  (safe-apply-n op (list x y) @ts?))

(define (lift-op op . ts)
  (case (procedure-arity op)
    [(1)  (lambda (x) (safe-apply-1 op x ts))]
    [(2)  (lambda (x y) (safe-apply-2 op x y ts))]
    [else
     (case-lambda
       [() (op)]
       [(x) (safe-apply-1 op x ts)]
       [(x y) (safe-apply-2 op x y ts)]
       [xs (safe-apply-n op xs ts)])]))

(define T*->string? (const @string?))

;; ----------------- String Operators ----------------- ;;

; TODO: Simplifications

(define (string-append-simplify xs)
  (match xs
    [(list) xs]
    [(list _) xs]
    [(list-rest (? string? x) ..2 rest)
     (list* (apply string-append x) (string-append-simplify rest))]
    [(list x rest ...)
     (list* x (string-append-simplify rest))]))

(define ($string-append . xs)
  (match xs
    [`() ""]
    [(list x) x]
    [(list x y)
     (match* (x y)
       [((? string?) (? string?)) (string-append x y)]
       [(_ _) (expression @string-append x y)])]
    [_
     (match (string-append-simplify xs)
       [(list x) x]
       [ys (apply expression @string-append ys)])]))

(define-operator @string-append
  #:identifier 'string-append
  #:range T*->T
  #:unsafe $string-append
  #:safe (lift-op $string-append))

(define ($string-length s)
  (match s
    [(? string? x) (string-length x)]
    [x (expression @string-length x)]))

(define-operator @string-length
  #:identifier 'string-length
  #:range T*->integer?
  #:unsafe $string-length
  #:safe (lift-op $string-length))

(define (integer->string i)
  (match i
    [(? integer? x) (number->string x)]
    [x (expression @integer->string x)]))
  
(define-operator @integer->string
  #:identifier 'integer->string
  #:range T*->string?
  #:unsafe integer->string
  #:safe (lift-op integer->string @integer?))

(define (string->integer s)
  (match s
    [(? string? x)
     (let ((n (string->number s)))
       (if (integer? n) n #f))] 
    [x (expression @string->integer x)]))

(define-operator @string->integer
  #:identifier 'string->integer
  #:range T*->integer?
  #:unsafe string->integer
  #:safe (lift-op string->integer))

(define ($substring s i [j (@string-length s)])
  (if (and (string? s) (number? i) (number? j)) 
      (substring s i j)
      (expression @substring s i j)))

(define (guarded-substring s i [j (@string-length s)])
  (assert (&& (@>= i 0) (@<= i j) (@<= j (@string-length s)))
          (thunk (error 'substring "index out of bounds")))
  ($substring s i j))
  
(define-operator @substring 
  #:identifier 'substring
  #:range T*->string?
  #:unsafe $substring
  #:safe (lift-op guarded-substring @string? @integer? @integer?))

(define ($string-contains? s p)
  (if (and (string? s) (string? p))
      (string-contains? s p)
      (expression @string-contains? s p)))

(define-operator @string-contains?
  #:identifier 'string-contains?
  #:range T*->boolean? 
  #:unsafe $string-contains?
  #:safe (lift-op $string-contains?))

; TODO match racket semantics, but for now, don't support #t (unsure how to encode)
(define ($string-replace s from to [all? #t])
  (if (and (string? s) (string? from) (string? to))
      (string-replace s from to #:all? all?)
      (expression @string-replace-internal s from to all?)))

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
     (type-cast @string? from caller)  ;TODO for now, only accepts strings, eventually needs or/c string? regexp? 
     (type-cast @string? to caller)
     all?)))

(define (@string-replace s from to #:all? [all? #t])
  (@string-replace-internal s from to all?))

(define ($string-prefix? s pre)
  (match* (s pre)
    [((? string?) (? string?)) (string-prefix? s pre)]
    [(_ _) (expression @string-prefix? s pre)]))
 
(define-operator @string-prefix?
  #:identifier 'string-prefix?
  #:range T*->boolean? 
  #:unsafe $string-prefix?
  #:safe (lift-op $string-prefix?))

(define ($string-suffix? s suf)
  (if (and (string? s) (string? suf))
      (string-suffix? s suf)
      (expression @string-suffix? s suf)))
              
(define-operator @string-suffix?
  #:identifier 'string-suffix?
  #:range T*->boolean?
  #:unsafe $string-suffix?
  #:safe (lift-op $string-suffix?))

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

(define (index-of s sub offset)
  (if (@string-contains? s sub)
     (let ([len-sub (@string-length sub)] [len (@string-length s)])
       (for/or ([i (in-range offset (+ (- len len-sub) 1))])
         (if (@string-contains? (@substring s i (+ i len-sub)) sub)
             i
             #f)))
     -1))

(define (string-index-of s sub [offset 0])
  (if (and (string? s) (string? sub) (number? offset))
      (index-of s sub offset)
      (expression @string-index-of s sub offset)))

(define (guarded-string-index-of s sub [offset 0])
  (assert (&& (@<= 0 offset) (@< offset (@string-length s))) (thunk (error 'string-index-of "offset out of bounds")))
  (string-index-of s sub offset))

(define-operator @string-index-of
  #:identifier 'string-index-of
  #:range T*->integer?
  #:unsafe string-index-of
  #:safe (lift-op guarded-string-index-of @string? @string? @integer?))

; We are going to disable all mutation operations on strings.

(define disable-mutation (lambda xs (error 'string-set! "string mutation not supported")))

(define @string-set! (impersonate-procedure string-set! disable-mutation))    
(define @string-fill! (impersonate-procedure string-fill! disable-mutation))
(define @string-copy! (impersonate-procedure string-copy! disable-mutation))

; TODO tests