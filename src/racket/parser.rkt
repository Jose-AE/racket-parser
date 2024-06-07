#lang racket

(define (read-and-print-file file-path)
  (with-input-from-file file-path (lambda () (display (port->string (current-input-port))))))

;; Call the function with the path to your .txt file
(read-and-print-file "src/racket/input/source_code.txt")

(display "<p style=\"color: #DC143C	;\">-parsedByRacket</p>")
