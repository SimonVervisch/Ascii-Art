#lang racket/base

(define (write-in-ascii string)
  
  (let ((line-length 40)
        (space-length 3)
        (string (string-upcase string)))
    (cond ((< (string-length string) (- line-length 2))
           (define (display-char n char)
             (when (> n 0)
               (display char)
               (display-char (- n 1) char)))
           (define (display-line-with-string)
      
             (let* ((middle-char-length (+ (* 2 space-length)
                                           (string-length string)))
                    (left-side-char-length (quotient (- line-length
                                                        middle-char-length) 2))
                    (right-side-char-length (- line-length left-side-char-length middle-char-length)))
               (display-char left-side-char-length ";")
               (display-char space-length " ")
               (display string)
               (display-char space-length " ")
               (display-char right-side-char-length ";")))
           (display-char line-length ";")
           (newline)
           (display-line-with-string)
           (newline)
           (display-char line-length ";"))
          (else (display "string too long")))))


(display "Insert 'stop' to finish")
(newline)
(let loop
  ((x (symbol->string (read))))
  (if (string=? x "stop")
      (display "No more input")
      (begin (write-in-ascii x)
             (newline)
             (loop (symbol->string (read))))))

