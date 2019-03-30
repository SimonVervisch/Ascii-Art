#lang racket


(define (write-in-ascii string)
  (let ((line-length 40)
        (space-length 3))
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
     (display-char line-length ";")))

