#lang racket

(require "term.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" "real.rkt")

(provide @string? @string-append @string-length @substring @string-ref
         @string-contains? @string-prefix? @string-suffix? @string-replace
         @str-to-int @int-to-str @string-set! @string-fill! @string-copy!)

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
       [(? string?) (values #t v)]
       [(term _ (== @string?)) (values #t v)]
       [(union : [g (and (app type-of (== @string?)) u)] _ ...) (values g u)]
       [_ (values #f v)]))
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

(define (string-append-simplify xs)
  (match xs
    [(list) xs]
    [(list _) xs]
    [(list-rest (? string? x) ..2 rest)
     (list* (apply string-append x) (string-append-simplify rest))]
    [(list x rest ...) (list* x (string-append-simplify rest))]))

; TODO refactor to lift-op like other stuff

; Operators

(define T*->string? (const @string?))

(define-operator @string-append
  #:identifier 'string-append
  #:range T*->T
  #:unsafe @string-append
  #:safe
  (case-lambda
    [() ""]
    [(x) x]
    [(x y)
     (match* (x y)
       [((? string?) (? string?)) (string-append x y)]
       [(_ _) (expression @string-append x y)])]
    [xs
     (match (string-append-simplify xs)
       [(list x) x]
       [ys (apply expression @string-append ys)])]))

(define-operator @string-length
  #:identifier 'string-length
  #:range T*->integer?
  #:unsafe @string-length
  #:safe
  (match-lambda
    [(? string? x) (string-length x)]
    [x (expression @string-length x)]))

(define (int-to-str i)
  (if (and i (integer? i) (>= i 0))
      (number->string i)
      ""))

(define-operator @int-to-str
  #:identifier 'int-to-str
  #:range T*->string?
  ;#:unsafe TODO
  #:safe
  (match-lambda
    [(? number? x) (int-to-str x)]
    [x (expression @int-to-str x)]))

(define (str-to-int s)
  (let ((n (string->number s)))
    (if (and n (integer? n) (>= n 0)) n -1)))

(define-operator @str-to-int
  #:identifier 'str-to-int
  #:range T*->integer?
  ;#:unsafe TODO
  #:safe
  (match-lambda [(? string? x) (str-to-int x)]
                [x (expression @str-to-int x)]))

(define-operator @substring
  #:identifier 'substring
  #:range T*->string?
  ;#:unsafe TODO
  ; TODO #:pre  (case-lambda [(s i) (&& (@>= i 0) (@<= i (@string-length s)))]
                      ;[(s i j) (&& (@>= i 0) (@<= i j) (@<= j (@string-length s)))])
  #:safe
  (lambda (s i [j (@string-length s)])
    (if (and (string? s) (number? i) (number? j)) 
        (substring s i j)
        (expression @substring s i j))))

(define-operator @string-contains?
  #:identifier 'string-contains?
  #:range T*->boolean? 
  ;#:unsafe TODO
  #:safe
  (lambda (s p)
    (if (and (string? s) (string? p))
	(string-contains? s p)
	(expression @string-contains? s p))))

(define-operator @string-replace
  #:identifier 'string-replace
  #:range T*->T
  ;#:unsafe TODO
  #:safe
  (lambda (s from to)
    (if (and (string? s) (string? from) (string? to))
	(string-replace s from to)
	(expression @string-replace s from to))))

(define-operator @string-prefix?
  #:identifier 'string-prefix?
  #:range T*->boolean? 
  ;#:unsafe TODO
  #:safe
  (lambda (x y)
    (match* (x y)
      [((? string?) (? string?)) (string-prefix? x y)]
      [(_ _) (expression @string-prefix? x y)])))
              
(define-operator @string-suffix?
  #:identifier 'string-suffix?
  #:range T*->boolean?
  ;#:unsafe TODO
  #:safe
  (lambda (s p)
    (if (and (string? s) (string? p))
	(string-suffix? s p)
	(expression @string-suffix? s p))))

;(define-operator @string-ref TODO
  ;#:identifier 'string-ref
  ;#:range T*->T ;TODO 
  ;#:unsafe TODO
  ;#:safe TODO
  ;)

; We are going to disable all mutation operations on strings. TODO? Do I need this?

(define disable-mutation (lambda xs (error 'string-set! "string mutation not supported")))

(define @string-set! (impersonate-procedure string-set! disable-mutation))    
(define @string-fill! (impersonate-procedure string-fill! disable-mutation))
(define @string-copy! (impersonate-procedure string-copy! disable-mutation))