#lang racket

; Parse a Racket regexp into a format Z3 can handle
;
; See Racket: https://docs.racket-lang.org/reference/regexp.html
; See Z3: http://rise4fun.com/Z3/tutorial/sequences

(require parser-tools/lex
         parser-tools/lex-sre
         parser-tools/cfg-parser
         (prefix-in $ "../../solver/smt/smtlib2.rkt"))

(define-tokens res (LIT))

(define-empty-tokens ops (UNION ? * + LP RP LB RB LBN ANY ^ $ LMODE : FIRST - LOOKE LOOKN LOOKP LOOKNP i Di s Ds m Dm ESC EOF))

(define regexp-lexer
  (lexer
   ("(?<=" (token-LOOKP))
   ("(?<!" (token-LOOKNP))
   ("(?=" (token-LOOKE))
   ("(?!" (token-LOOKN))
   ("(?>" (token-FIRST))
   ("(?" (token-LMODE))
   ("[^" (token-LBN))
   ("|" (token-UNION))
   ("?" (token-?))
   ("*" (token-*))
   ("+" (token-+))
   ("?" (token-?))
   ("(" (token-LP))
   (")" (token-RP))
   ("[" (token-LB))
   ("]" (token-RB))
   ("." (token-ANY))
   ("^" (token-^))
   ("$" (token-$))
   (":" (token-:))
   ("-i" (token-Di))
   ("i" (token-i))
   ("-s" (token-Ds))
   ("s" (token-s))
   ("-m" (token-Dm))
   ("m" (token-m))
   ("-" (token--))
   ("\\" (token-ESC))
   (any-char (token-LIT lexeme))
   ((eof) (token-EOF))))

; TODO better error messaging
(define (unsupported-regexp-error)
  (error (raise-syntax-error #f "provided regexp syntax not yet supported;\n")))

(define regexp-parser
  (cfg-parser
   (start re)
   (end EOF)
   (tokens res ops)
   (error unsupported-regexp-error)
   (grammar
    (re
     ((re UNION re) ($re.union $1 $3))
     ((pces) $1))
    (pces
     ((pce) $1)
     ((pce pces) ($re.++ $1 $2)))
    (pce
     ((repeat) $1) ; TODO since we don't support match, we don't care if it's shortest/longest, but may eventually
     ((repeat ?) $1) ; TODO since we don't support match, we don't care if it's shortest/longest, but may eventually
     ((atom) $1))
    (repeat
     ((atom *) ($re.* $1))
     ((atom +) ($re.+ $1))
     ((atom ?) ($re.opt $1)))
    (atom
     ((LP re RP) $2)
     ((LB rng RB) $2) 
     ((LBN rng RB) (unsupported-regexp-error)) ; TODO complement not yet supported in Z3, can't do algebraically, may need to bump up and handle earlier in enc as not
     ((ANY) (unsupported-regexp-error)) ; TODO any single character; somehow need both allchar and length is one, but can't nest that in an re
     ((^) (unsupported-regexp-error)) ; TODO start
     (($) (unsupported-regexp-error)) ; TODO finish
     ((lit) $1)
     ((LMODE mode : re RP) (unsupported-regexp-error)) ; TODO Match ‹regexp› using ‹mode›
     ((FIRST re RP) $2) ; TODO since we don't support match, we don't care if it's first, but may eventually
     ((look) $1)
     ((LMODE tst pces UNION pces RP) (unsupported-regexp-error)) ; TODO match 1st ‹pces› if ‹tst›, else 2nd ‹pces›
     ((LMODE tst pces RP) (unsupported-regexp-error))) ; TODO match ‹pces› if ‹tst›, empty if not ‹tst›
    (rng
     ((RB) ($str.to.re "]"))
     ((-) ($str.to.re "-"))
     ((mrng) $1)
     ((mrng -) ($re.union $1 ($str.to.re "-"))))
    (mrng
     ((RB lrng) ($re.union $2 ($str.to.re "]")))
     ((- lrng) ($re.union $2 ($str.to.re "-")))
     ((lirng) $1))
    (lirng
     ((rilit) $1)
     ((rilit - rilit) ($re.range $1 $3))
     ((lirng lrng) ($re.union $1 $2)))
    (lrng
     ((^) ($str.to.re "^"))
     ((rlit - rlit) ($re.range $1 $3))
     ((^ lrng) ($re.union ($str.to.re "^") $2))
     ((lirng) $1))
    (look
     ((LOOKE re RP) $2)
     ((LOOKN re RP) (unsupported-regexp-error)) ; TODO negation not yet supported in Z3, may need to bump up and handle earlier in enc
     ((LOOKP re RP) (unsupported-regexp-error)) ; TODO Match if ‹regexp› matches preceding (what does this mean?)
     ((LOOKNP re RP) (unsupported-regexp-error))) ; TODO negation not yet supported in Z3, may need to bump up and handle earlier in enc
    (tst
     ((LP LIT RP) (unsupported-regexp-error)) ; TODO true if Nth ( has a match
     ((look) $1))
    (mode
     (() (unsupported-regexp-error))
     ((mode i) (unsupported-regexp-error))
     ((mode s) (unsupported-regexp-error))
     ((mode m) (unsupported-regexp-error))
     ((mode Di) (unsupported-regexp-error))
     ((mode Ds) (unsupported-regexp-error))
     ((mode Dm) (unsupported-regexp-error)))
    (lit
     ((LIT) ($str.to.re $1))
     ((i) ($str.to.re "i"))
     ((s) ($str.to.re "s"))
     ((m) ($str.to.re "m"))
     ((-) ($str.to.re "-"))
     ((RB) ($str.to.re "]"))
     ((:) ($str.to.re ":"))
     ((ESC alit) ($str.to.re (string-append "\\" $2))))
    (alit
     ((rilit) $1)
     ((RB) ($str.to.re "]"))
     ((-) ($str.to.re "-"))
     ((^) ($str.to.re "^")))
    (rlit
     ((rilit) $1)
     ((^) ($str.to.re "^")))
    (rilit
     ((LIT) ($str.to.re $1))
     ((LP) ($str.to.re "("))
     ((RP) ($str.to.re ")"))
     ((*) ($str.to.re "*"))
     ((+) ($str.to.re "+"))
     ((?) ($str.to.re "?"))
     ((LB) ($str.to.re "["))
     ((ANY) ($str.to.re "."))
     ((ESC) ($str.to.re "\\"))
     ((UNION) ($str.to.re "|"))
     ((i) ($str.to.re "i"))
     ((s) ($str.to.re "s"))
     ((m) ($str.to.re "m"))
     ((:) ($str.to.re ":"))))))

; Test:
;(define (lex-this lexer input) (lambda () (lexer input)))

;(let ((input (open-input-string "a|b")))
  ;(regexp-parser (lex-this regexp-lexer input)))

;(let ((input (open-input-string "[a-zA-Z]")))
  ;(regexp-parser (lex-this regexp-lexer input)))