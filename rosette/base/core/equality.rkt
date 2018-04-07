#lang racket

(require "term.rkt" "union.rkt" "bool.rkt")

(provide @eq?     ; (-> any/c any/c @boolean?)
         @equal?) ; (-> any/c any/c @boolean?)


(define-syntax-rule (define-equality-predicate @=? =? type=? @cache @make-hash)
  (define (@=? x y)
    (let* ([cache (@cache)]
           [toplevel? (false? cache)]
           [key (cons x y)])
      (when toplevel?
        (set! cache (@make-hash))
        (@cache cache))
      (if (hash-has-key? cache key)
          (hash-ref cache key)
          (begin
            (hash-set! cache key #t)
            (let ([result
                   (cond [(=? x y) #t]
                         [(union? x) (if (union? y) 
                                         (union=union? x y @=?) 
                                         (union=value? x y @=?))]
                         [(union? y) (union=value? y x @=?)]

                         ;; Some tailored simplifications #vomit
                         [(string? x)
                          (match y
                            [(expression val prefix _)
                              (let* ([val-to-str (format "~v" val)])
                                (match val-to-str
                                  ["string-append"
                                    (cond
                                      [(and (string? prefix) (> (string-length x) 0) (> (string-length prefix) 0))
                                        (let* ([fx (string-ref x 0)] [fp (string-ref prefix 0)]
                                          [result (if (equal? fx fp) (type=? (type-of x y) x y) #f)])
                                          ;;(println (format "= for string-append: ~v ~v (~v, ~v)" x y fx fp))
                                          ;;(println (format "Result: ~v" result))
                                          result
                                        )
                                      ]
                                      [#t (type=? (type-of x y) x y)]
                                    )
                                  ]
                                  [_ (type=? (type-of x y) x y)]
                                )
                              )
                            ]
                            [_ (type=? (type-of x y) x y)]
                          )
                         ]

                         [(and (expression? x) (expression? y))
                          (match* (x y)
                            [((expression v1 p1 q1) (expression v2 p2 q2))
                              (let* ([vs1 (format "~v" v1)] [vs2 (format "~v" v2)])
                                (cond
                                  [(and (equal? vs1 "string-append") (equal? vs2 "string-append") (equal? p1 p2))
                                    (@=? q1 q2)]
                                  [#t (type=? (type-of x y) x y)]
                                )
                              )
                            ]
                            [(_ _) (type=? (type-of x y) x y)]
                          )
                         ]
                         
                         ;; Back to classic Rosette
                         [else (type=? (type-of x y) x y)])])
              (if toplevel?
                  (@cache #f)
                  (hash-set! cache key result))
              result))))))
                   
(define equal-cache (make-parameter #f))
(define eq-cache (make-parameter #f))
(define-equality-predicate @equal? equal? type-equal? equal-cache make-hash)
(define-equality-predicate @eq? eq? type-eq? eq-cache make-hasheq)

; (-> union? union? (-> any/c any/c @boolean?) @boolean?)
(define (union=union? x y =?)
  (match* (x y)
    [((union vs t) (union ws s))
     (and (or (subtype? t s) (subtype? s t))
          (apply || (for*/list ([v vs] [w ws]) 
                      (and-&&
                       (=? (cdr v) (cdr w))
                       (car v)
                       (car w)))))]))

; (-> union? (not/c union?) (-> any/c any/c @boolean?) @boolean?)
(define (union=value? x y =?)
  (match* (x y)
    [((union vs t) (app type-of s))
     (and (or (subtype? t s) (subtype? s t))
          (apply || (for/list ([v vs]) (and-&& (=? y (cdr v)) (car v)))))]))

