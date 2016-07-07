#lang racket

(require "term.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" "real.rkt"
         "merge.rkt" "safe.rkt")

(provide @string? @string-append @string-length @substring ;@string-ref TODO
         @string-contains? @string-prefix? @string-suffix? @string-replace
         @str-to-int @int-to-str @string-at)

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
       [(union : [g (and (app type-of (== @string?)) u)] _ ...) (values g u)] ;TODO
       [_ (assert #f)])) ;TODO
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

(define (string-append-simplify xs)
  (match xs
    [(list) xs]
    [(list _) xs]
    [(list-rest (? string? x) ..2 rest)
     (list* (apply string-append x) (string-append-simplify rest))]
    [(list x rest ...) (list* x (string-append-simplify rest))]))

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

(define (int-to-str i)
  (match i
    [(? number? x)
     (if (and x (integer? x) (>= x 0))
      (number->string x)
      "")]
    [x (expression @int-to-str x)]))
  
(define-operator @int-to-str
  #:identifier 'int-to-str
  #:range T*->string?
  #:unsafe int-to-str
  #:safe (lift-op int-to-str @integer?))

(define (str-to-int s)
  (match s
    [(? string? x)
     (let ((n (string->number s)))
       (if
        (and n (integer? n) (>= n 0))
        n
        0))] ; TODO old behavior is -1, Racket is #f, Z3 is 0; what do we want?
    [x (expression @str-to-int x)]))

(define-operator @str-to-int
  #:identifier 'str-to-int
  #:range T*->integer?
  #:unsafe str-to-int
  #:safe (lift-op str-to-int))

(define ($substring s i [j (@string-length s)])
  (if (and (string? s) (number? i) (number? j)) 
      (substring s i j)
      (expression @substring s i j)))

(define (guarded-substring s i [j (@string-length s)])
  (assert (&& (@>= i 0) (@<= i j) (@<= j (@string-length s))))
  (substring s i j))
  
(define-operator @substring 
  #:identifier 'substring
  #:range T*->string?
  #:unsafe $substring
  #:safe (lift-op guarded-substring @string? @integer? @integer?))

(define ($string-contains? s p) ; TODO refactor out this pattern
  (if (and (string? s) (string? p))
      (string-contains? s p)
      (expression @string-contains? s p)))

(define-operator @string-contains?
  #:identifier 'string-contains?
  #:range T*->boolean? 
  #:unsafe $string-contains?
  #:safe (lift-op $string-contains?))

(define ($string-replace s from to #:all? [all? #t])
  (if (and (string? s) (string? from) (string? to))
      (string-replace s from to #:all? all?)
      (expression @string-replace s from to)))

(define-operator @string-replace
  #:identifier 'string-replace
  #:range T*->string?
  #:unsafe $string-replace
  #:safe
  (lambda (s from to #:all? [all? #t])
    (define caller 'string-replace)
    ($string-replace
     (type-cast @string? s caller)
     (type-cast @string? from caller)  ;TODO for now, only accepts strings, eventually needs or/c string? regexp? 
     (type-cast @string? to caller)
     #:all? all?))) ; TODO do I need to TC this if it's an any/c in Racket?

(define ($string-prefix? x y)
  (match* (x y)
    [((? string?) (? string?)) (string-prefix? x y)]
    [(_ _) (expression @string-prefix? x y)]))
 
(define-operator @string-prefix?
  #:identifier 'string-prefix?
  #:range T*->boolean? 
  #:unsafe $string-prefix?
  #:safe (lift-op $string-prefix?))

(define ($string-suffix? s p)
  (if (and (string? s) (string? p))
      (string-suffix? s p)
      (expression @string-suffix? s p)))
              
(define-operator @string-suffix?
  #:identifier 'string-suffix?
  #:range T*->boolean?
  #:unsafe $string-suffix?
  #:safe (lift-op $string-suffix?))

(define (string-at s offset)
  (if (and (string? s) (number? offset))
      (@substring s offset (+ offset 1))
      (expression @string-at s offset)))

(define-operator @string-at
  #:identifier 'string-at
  #:range T*->string?
  #:unsafe string-at
  #:safe (lift-op string-at @string? @integer?))

; TODO index-of
;(define (index-of s sub offset)
;)

;(define (string-index-of s sub [offset 0])
  ;(if (and (string? s) (string? sub) (number? offset))
      ;(index-of s sub offset)
      ;(expression @string-index-of s sub offset)))

; We are going to disable all mutation operations on strings. TODO? Do I need this, since I don't need those methods?

;(define disable-mutation (lambda xs (error 'string-set! "string mutation not supported")))

;(define @string-set! (impersonate-procedure string-set! disable-mutation))    
;(define @string-fill! (impersonate-procedure string-fill! disable-mutation))
;(define @string-copy! (impersonate-procedure string-copy! disable-mutation))