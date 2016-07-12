#lang racket

(require parser-tools/lex
         parser-tools/lex-sre
         parser-tools/yacc)

; ALIT RILIT RLIT
; REGEXP PCES PCE REPEAT ATOM RNG MRNG LIRNG LRNG LOOK TST MODE 

(define-tokens res (LIT))

(define-empty-tokens ops (UNION ? * + LP RP LB RB ANY ^ $ LMODE : FIRST - LOOKE LOOKN LOOKP LOOKNP i Di s Ds m Dm ESC))

(define regexp-lexer
  (lexer
   ("(?<=" (token-LOOKP))
   ("(?<!" (token-LOOKNP))
   ("(?=" (token-LOOKE))
   ("(?!" (token-LOOKN))
   ("(?>" (token-FIRST))
   ("(?" (token-LMODE))
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
   ;TODO literals
   ))