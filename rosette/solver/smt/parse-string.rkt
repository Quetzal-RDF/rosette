#lang racket

(provide string->$str escape-string)

; Encode a string literal into the UTF-8 format that Z3 expects
; Credit to georges-duperon from the Racket IRC for writing most of this
(define (string->$str s)
  (escape-string
   (bytes->string/utf-8
    (apply bytes-append
           (for/list ([b (string->bytes/utf-8 s)])
             (cond
               [(and (>= b 32) (<= b 126)) (bytes b)]
               [(< b #x10)
                (string->bytes/utf-8 (format "\\x0~x" b))]
               [(or (and (>= b #x10) (< b 32)) (> b 126))
                (string->bytes/utf-8 (format "\\x~x" b))]))))))

(define (escape-string s)
  (string-append "\"" s "\""))