#lang racket

(require "lexer.rkt")
(require parser-tools/yacc)

(define (increment x) 
  (cond [(number? x) (+ x 1)]
        [(symbol? x) (list 'post-increment x)]))

(define (decrement x)
  (cond [(number? x) (- x 1)]
        [(symbol? x) (list 'post-decrement x)]))

(define error-handler
  (lambda (tok-ok? tok-name tok-value start-pos end-pos)
    (printf "Sintaxis incorrecta")
    (printf "\n\n\nError: Invalid token detected: ~a, Value: ~a\n\n\n\n" tok-name tok-value)))

(define the-parser
  (parser [start c-program]
          [end EOF]
          [src-pos]
          [error error-handler]
          [tokens
           literal-tokens
           identifier-tokens
           keyword-tokens
           type-tokens
           expression-operator-tokens
           term-operator-tokens
           punctuation-tokens]
          [grammar 
           [c-program [(main-function) $1]]
           [main-function [(INT IDENTIFIER LEFT_PAREN RIGHT_PAREN block)
                          (if (equal? $2 "main")
                              (list 'main-function $5)
                              (error "Expected 'main' function"))]]
           [block [(LEFT_BRACE statements RIGHT_BRACE) $2]]
           [statements [(statement statements) (cons $1 $2)]
                       [() '()]]
           [statement [(declaration SEMICOLON) $1]
                      [(expr SEMICOLON) $1]
                      [(if-statement) $1]
                      [(for-statement) $1]
                      [(while-statement) $1]
                      [(do-while-statement) $1]
                      [(switch-statement) $1]
                      [(return-statement) $1]
                      [(continue-statement) $1]
                      [(function-call SEMICOLON) $1]]
           [if-statement [(IF LEFT_PAREN expr RIGHT_PAREN block) 
                         (list 'if $3 $5)]
                        [(IF LEFT_PAREN expr RIGHT_PAREN block ELSE block) 
                         (list 'if-else $3 $5 $7)]
                        [(IF LEFT_PAREN expr RIGHT_PAREN statement) 
                         (list 'if $3 $5)]
                        [(IF LEFT_PAREN expr RIGHT_PAREN statement ELSE statement) 
                         (list 'if-else $3 $5 $7)]]
           [for-statement [(FOR LEFT_PAREN for-init SEMICOLON for-cond SEMICOLON for-incr RIGHT_PAREN block)
                         (list 'for $3 $5 $7 $9)]
                        [(FOR LEFT_PAREN for-init SEMICOLON for-cond SEMICOLON for-incr RIGHT_PAREN statement)
                         (list 'for $3 $5 $7 $9)]]
           [while-statement [(WHILE LEFT_PAREN expr RIGHT_PAREN block)
                             (list 'while $3 $5)]
                            [(WHILE LEFT_PAREN expr RIGHT_PAREN statement)
                             (list 'while $3 $5)]]
           [do-while-statement [(DO block WHILE LEFT_PAREN expr RIGHT_PAREN SEMICOLON)
                                (list 'do-while $2 $5)]
                               [(DO statement WHILE LEFT_PAREN expr RIGHT_PAREN SEMICOLON)
                                (list 'do-while $2 $5)]]
           [switch-statement [(SWITCH LEFT_PAREN expr RIGHT_PAREN switch-block) 
                              (list 'switch $3 $5)]]
           [switch-block [(LEFT_BRACE case-statements RIGHT_BRACE) $2]]
           [case-statements [(case-statement case-statements) (cons $1 $2)]
                            [(default-statement case-statements) (cons $1 $2)]
                            [() '()]]
           [case-statement [(CASE expr COLON statements break-statement) 
                            (list 'case $2 $4 $5)]]
           [default-statement [(DEFAULT COLON statements break-statement) 
                              (list 'default $3 $4)]
                             [(DEFAULT COLON statements) 
                              (list 'default $3)]]
           [break-statement [(BREAK SEMICOLON) 'break]]
           [return-statement [(RETURN expr SEMICOLON) 
                             (list 'return $2)]
                            [(RETURN SEMICOLON)
                             'return]]
           [continue-statement [(CONTINUE SEMICOLON) 'continue]]
           [for-init [(declaration) $1]
                     [(expr) $1]
                     [() #f]]
           [for-cond [(expr) $1]
                     [() #t]]
           [for-incr [(expr) $1]
                     [() #f]]
           [function-call [(IDENTIFIER LEFT_PAREN args RIGHT_PAREN) (list 'call $1 $3)]]
           [args [(expr) (list $1)]
                 [() '()]]
           [declaration [(type-token IDENTIFIER) (list 'declare $1 $2)]
                       [(type-token IDENTIFIER ASSIGNMENT expr) (list 'declare-init $1 $2 $4)]]
           [type-token [(INT) 'int]
                       [(CHAR) 'char]
                       [(FLOAT) 'float]
                       [(DOUBLE) 'double]
                       [(VOID) 'void]]
           [expr [(IDENTIFIER ASSIGNMENT expr) (list 'assign $1 $3)]
                 [(expr PLUS expr) (list '+ $1 $3)]
                 [(expr MINUS expr) (list '- $1 $3)]
                 [(expr MULTIPLY expr) (list '* $1 $3)]
                 [(expr DIVIDE expr) (list '/ $1 $3)]
                 [(expr EQUAL expr) (list '== $1 $3)]
                 [(expr NOT_EQUAL expr) (list '!= $1 $3)]
                 [(expr LESS_THAN expr) (list '< $1 $3)]
                 [(expr GREATER_THAN expr) (list '> $1 $3)]
                 [(expr LESS_THAN_OR_EQUAL expr) (list '<= $1 $3)]
                 [(expr GREATER_THAN_OR_EQUAL expr) (list '>= $1 $3)]
                 [(expr AND expr) (list '&& $1 $3)]
                 [(expr OR expr) (list '|| $1 $3)]
                 [(expr INCREMENT) (increment $1)]
                 [(expr DECREMENT) (decrement $1)]
                 [(IDENTIFIER) $1]
                 [(NUMBER) $1]
                 [(LEFT_PAREN expr RIGHT_PAREN) $2]
                 [(STRING) $1]]
           [term [(IDENTIFIER) $1]
                 [(NUMBER) $1]
                 [(STRING) $1]
                 [(LEFT_PAREN expr RIGHT_PAREN) $2]
                 [(expr INCREMENT) (increment $1)]
                 [(expr DECREMENT) (decrement $1)]]
           ]))

;; Function to generate highlighted HTML
(define (surround1 s1 c)
  (string-append "<" c ">" s1))

(define (surround2 s1 c)
  (string-append "<" c ">" s1 "</" c ">"))

(define (surround3 s1 c)
  (string-append s1 "</" c ">"))

(define (sep str)
  (regexp-split #rx"(?<=\\]|\\[|(\r\n)|[+-/()<>{}=;: ])|(?=\\]|\\[|(\r\n)|[+-/()<>{}=;: ])" str))

(define (surroundRegexp s1)
  (cond
    [(regexp-match #rx"[0-9]+" s1) (set! s1 (string-append "<span class=\"number\">" s1 "</span>"))]
    [(regexp-match #rx"^int$|^char$|^double$|^void$" s1) (set! s1 (string-append "<span class=\"type\">" s1 "</span>"))]
    [(regexp-match #rx"\\." s1) (set! s1 (string-append "<span class=\"number\">" s1 "</span>"))]
    [(regexp-match #rx"[><*=/+-]" s1) (set! s1 (string-append "<span class=\"operator\">" s1 "</span>"))]
    [(regexp-match #rx"^else$|^if$" s1) (set! s1 (string-append "<span class=\"keyword\">" s1 "</span>"))]
    [(regexp-match #rx"^for$|^while$|^switch$|^do|^case$|^default$|^break$" s1) (set! s1 (string-append "<span class=\"keyword\">" s1 "</span>"))]
    [(regexp-match #rx"\".*\"" s1) (set! s1 (string-append "<span class=\"literal\">" s1 "</span>"))]
    [(regexp-match #rx"(?<=\")(?=.)" s1) (set! s1 (string-append "<span class=\"literal\">" s1))]
    [(regexp-match #rx"(?<=.)(?=\")" s1) (set! s1 (string-append s1 "</span>"))]
    [(regexp-match #rx"(?<=')(?=.)" s1) (set! s1 (string-append "<span class=\"literal\">" s1))]
    [(regexp-match #rx"(?<=.)(?=')" s1) (set! s1 (string-append s1 "</span>"))]
    [(regexp-match #rx"'.*'" s1) (set! s1 (string-append "<span class=\"literal\">" s1 "</span>"))]
    [(regexp-match #rx"\\]|\\[|[(]|[)]|[{]|[}]|[;]" s1) (set! s1 (string-append "<span class=\"punctuation\">" s1 "</span>"))]
    [(regexp-match #rx"^printf$|^main$" s1) (set! s1 (string-append "<span class=\"function\">" s1 "</span>"))]
    [(regexp-match #rx"[,]" s1) (set! s1 (string-append "<span class=\"comma\">" s1 "</span>"))]
    [(regexp-match #rx" " s1) (set! s1 "&nbsp;")]
    [(regexp-match #rx"\r\n" s1) (set! s1 "<br>")]
    [(regexp-match #rx"\n" s1) (set! s1 "<br>")]
    [else s1])  ; Devolver s1 sin cambios si no coincide con ninguna regla
  s1)

(define (resaltar2 x)
  (set! x (sep x))
  (set! x (map (lambda (lst) (surroundRegexp lst)) x))
  (set! x (string-join x ""))
  (set! x (string-append " <html> <head> <link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\"> </head> <body> " x "</body> </html> "))
  x)

(define (res ruta string)
  (call-with-output-file ruta
    (lambda (port)
      (display string port))
    #:exists 'replace))

(define (resultado x)
  (res "resaltado.html" (resaltar2 x)))

(define (process-file file-path)
  (define src-code (open-input-file file-path))
  (port-count-lines! src-code)
  (define result (the-parser (λ () (lex src-code))))
  (define highlighted-code (resaltar2 (file->string file-path)))
  (close-input-port src-code)
  (cons result highlighted-code))

;; Parallel processing functions
(define (split lst n)
  (define len (length lst))
  (define size (max 1 (quotient len n)))
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (loop (drop lst size) (cons (take lst size) acc)))))

(define (process-files-parallel files n)
  (define split-files (split files n))
  (define futures (map (lambda (file-sublist)
                         (future (lambda () (map process-file file-sublist))))
                       split-files))
  (apply append (map touch futures)))

(define (process-files-sequential files)
  (map process-file files))

;; Function to generate a single HTML with all highlighted code
(define (generate-combined-html results)
  (string-append 
   "<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\"></head><body>"
   (string-join (map cdr results) "\n<hr>\n")
   "</body></html>"))

;; Main function to parse the source code and generate highlighted HTML
(define (main)
  (define test-files '("input/test1.c" "input/test2.c" "input/test3.c" "input/test4.c"))

  (printf "Procesando archivos secuencialmente:\n")
  (define-values (seq-results seq-cpu-time seq-cpu-time-exact seq-real-time)
    (time-apply process-files-sequential (list test-files)))
  (printf "Tiempo de CPU: ~a ms, Tiempo real: ~a ms\n" seq-cpu-time seq-real-time)

  (printf "\nProcesando archivos en paralelo (4 hilos):\n")
  (define-values (par-results par-cpu-time par-cpu-time-exact par-real-time)
    (time-apply process-files-parallel (list test-files 4)))
  (printf "Tiempo de CPU: ~a ms, Tiempo real: ~a ms\n" par-cpu-time par-real-time)

  (define combined-html (generate-combined-html (car seq-results)))
  (call-with-output-file "output/resaltado_combinado.html"
    (lambda (port)
      (display combined-html port))
    #:exists 'replace))

(main)