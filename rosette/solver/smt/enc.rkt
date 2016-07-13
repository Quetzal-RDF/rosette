#lang racket

(require "env.rkt"
         "parse-regexp.rkt"
         (prefix-in $ "smtlib2.rkt") 
         (only-in "../../base/core/term.rkt" expression expression? constant? get-type @app)
         (only-in "../../base/core/polymorphic.rkt" ite ite* =? guarded-test guarded-value)
         (only-in "../../base/core/bool.rkt" @! @&& @|| @=> @<=>)
         (only-in "../../base/core/real.rkt" 
                  @integer? @real? @= @< @<= @>= @> 
                  @+ @* @- @/ @quotient @remainder @modulo 
                  @abs @integer->real @real->integer @int?)
         (only-in "../../base/core/bitvector.rkt" 
                  bitvector? bv bitvector-size 
                  @bveq @bvslt @bvsle @bvult @bvule   
                  @bvnot @bvor @bvand @bvxor @bvshl @bvlshr @bvashr
                  @bvneg @bvadd @bvmul @bvudiv @bvsdiv @bvurem @bvsrem @bvsmod
                  @concat @extract @zero-extend @sign-extend 
                  @integer->bitvector @bitvector->integer @bitvector->natural)
         (only-in "../../base/core/string.rkt"
                  @string-append @string-length @substring
                  @string-contains? @string-prefix? @string-suffix?
                  @string-replace @string->integer @integer->string
                  @string-at @string-index-of)
         (only-in "../../base/core/regexp.rkt"
                  @regexp @regexp-quote @regexp-match-exact? @string->regexp
                  @regexp-all @regexp-none @regexp-concat @regexp-range
                  @regexp-star @regexp-plus @regexp-opt @regexp-loop
                  @regexp-union @regexp-inter))

(provide enc)

; The enc procedure takes a value and an environment, and returns  
; an SMTLIB identifier representing that value in the given environment.  If it 
; cannot produce an encoding for the given value, an error is thrown. 
; The environment will be modified (if needed) to include an encoding for 
; the given value and all of its subexpressions (if any).
(define (enc v env)
  (ref! env v (match v
                [(? expression?) (enc-expr v env)]
                [(? constant?)   (enc-const v env)]
                [_               (enc-lit v env)])))

(define (enc-expr v env)  
  (match v
    [(and (expression (== ite*) gvs ...) (app get-type t))
     (let-values ([($0 $op) (if (bitvector? t) 
                                (values ($bv 0 (bitvector-size t)) $bvor) 
                                (values 0 $+))])
       (apply $op (for/list ([gv gvs]) 
                    ($ite (enc (guarded-test gv) env) 
                          (enc (guarded-value gv) env) 
                          $0))))]
    [(expression (== @abs) x)
     ($real-abs (enc x env) (get-type v))]
    [(expression (== @extract) i j e)
     ($extract i j (enc e env))]
    [(expression (== @sign-extend) v t)
     ($sign_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @zero-extend) v t)
     ($zero_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @integer->bitvector) v t) 
     ($int->bv (enc v env) (bitvector-size t))]
    [(expression (== @bitvector->integer) v) 
     ($bv->int (enc v env) (bitvector-size (get-type v)))]  
    [(expression (== @bitvector->natural) v) 
     ($bv->nat (enc v env) (bitvector-size (get-type v)))]
    [(expression (== @substring) s i j)
     ($str.substr (enc s env) (enc i env) (- (enc (@string-length s) env) (enc j env)))]
    [(expression (== @string-replace) s from to all?)
     ($str.replace (enc s env) (enc from env) (enc to env))]
    [(expression (== @regexp-match-exact?) r s)
     ($str.in.re (enc s env) (enc r env))]
    [(expression (app rosette->smt (? procedure? $op)) es ...) 
     (apply $op (for/list ([e es]) (enc e env)))]
    [_ (error 'enc "cannot encode ~a to SMT" v)]))

; TODO @regexp-concat to re.++: Squish to 3 args
; TODO @regexp-loop to re.loop: Format arguments in the weird expected way

(define (enc-const v env) (ref! env v))

(define (enc-lit v env)
  (match v 
    [#t $true]
    [#f $false]
    [(? integer?) (inexact->exact v)]
    [(? real?) (if (exact? v) ($/ (numerator v) (denominator v)) v)]
    [(bv lit t) ($bv lit (bitvector-size t))]
    [(? string?) v]
    [(? regexp?) (parse-re v)]
    [_ (error 'enc "expected a boolean?, integer?, real?, bitvector?, string?, or regexp? given ~a" v)]))

(define-syntax define-encoder
  (syntax-rules ()
    [(_ id [rosette-op smt-op] ...)
     (define (id op) 
       (cond [(eq? op rosette-op) smt-op] ... 
             [else #f]))]))

(define-encoder rosette->smt 
  ; core 
  [@app $app] [@! $not] [@&& $and] [@|| $or] [@=> $=>] [@<=> $<=>] [ite $ite] [=? $=]
  ; int and real
  [@= $=] [@< $<] [@<= $<=] 
  [@+ $+] [@* $*] [@- $-] [@/ $/]  
  [@quotient $quotient] [@remainder $remainder] [@modulo $modulo]
  [@integer->real $to_real] [@real->integer $to_int] [@int? $is_int]
  ; bitvector
  [@bveq $=] [@bvslt $bvslt] [@bvsle $bvsle] [@bvult $bvult] [@bvule $bvule] 
  [@bvnot $bvnot] [@bvor $bvor] [@bvand $bvand] [@bvxor $bvxor] 
  [@bvshl $bvshl] [@bvlshr $bvlshr] [@bvashr $bvashr]
  [@bvneg $bvneg] [@bvadd $bvadd] [@bvmul $bvmul] [@bvudiv $bvudiv] [@bvsdiv $bvsdiv]
  [@bvurem $bvurem] [@bvsrem $bvsrem] [@bvsmod $bvsmod] [@concat $concat]
  ; string
  [@string-append $str.++] [@string-length $str.len]
  [@integer->string $int.to.str] [@string->integer $str.to.int]
  [@string-contains? $str.contains] [@string-prefix? $str.prefixof]
  [@string-suffix? $str.suffixof] [@string-at $str.at]
  [@string-index-of $str.indexof]
  ; regex
  [@string->regexp $str.to.re] [@regexp-range $re.range]
  [@regexp-star $re.*] [@regexp-plus $re.+] [@regexp-opt $re.opt]
  [@regexp-union $re.union] [@regexp-inter $re.inter])

; TODO @regexp @regexp-quote @regexp-all @regexp-nostr

; TODO: for some of these (like replace), where racket and Z3 defaults differ, may
; need a better encoding, will revisit once basic code is working


(define ($quotient tx ty)
  (define tx/ty ($div ($abs tx) ($abs ty)))
  ($ite ($= ($< tx 0) ($< ty 0)) tx/ty ($- tx/ty)))

(define ($remainder tx ty)
  (define tx%ty ($mod ($abs tx) ($abs ty)))
  ($ite ($< tx 0) ($- tx%ty) tx%ty))

(define ($modulo tx ty)
  ($ite ($< 0 ty) ($mod tx ty) ($- ($mod ($- tx) ty))))

(define ($real-abs v t)
  (if (equal? t @integer?)
      ($abs v)
      ($ite ($< v 0) ($- v) v)))

(define ($int->bv i n)
  (define bv0 ($bv 0 n))
  (apply 
   $bvor 
   (let loop ([b (- n 1)] [m ($mod i (expt 2 n))])
     (if (< b 0)
         (list)
         (let* ([2^b (expt 2 b)]
                [1? ($<= 2^b m)])          
           (cons ($ite 1? ($bv 2^b n) bv0) 
                 (loop (- b 1) ($- m ($ite 1? 2^b 0)))))))))

(define ($bv->nat v n) 
  (apply $+ (for/list ([i n]) ($bit v i n))))

(define ($bv->int v n)
  (apply $+ ($- ($bit v (- n 1) n)) (for/list ([i (- n 1)]) ($bit v i n))))

(define ($bit v i n)
  (define bv0 ($bv 0 n))
  (define b (expt 2 i))
  ($ite ($= bv0 ($bvand v ($bv b n))) 0 b))
