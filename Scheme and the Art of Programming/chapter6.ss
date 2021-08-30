(display '--------6.1)
(newline)

(define (substring? sstr strng)
    (let ((s1 (string-length sstr))
          (s2 (string-length strng)))
          (cond ((string=? sstr "") #t)
                ((> s1 s2) nil)
                ((= s1 s2) (string=? sstr strng))
                (else (or (string=? sstr (substring strng 0 s1)) (substring? sstr (substring strng 1 s2)))))))


(display (substring? "s a s" "This is a string."))
(newline)
(display (substring? "ringer" "This is a string."))
(newline)
(display (substring? "" "This is a string."))
(newline)

(display '--------6.2)
(newline)

(define (substring-ref n)
    (substring strng n (add1 n)))

(define (string-reverse s)
    (if (string=? s "")
        ""
        (string-append (string-reverse (substring s 1 (string-length s))) (substring s 0 1))))


(display (string-reverse "Jack and Jill"))
(newline)
(display (string-reverse "mom n dad"))
(newline)
(display (string-reverse ""))
(newline)

(display '--------6.3)
(newline)

(define (palindrome? s)
    (let ((s1 (string-length s)))
        (if (even? s1)
            (string=? (substring s 0 (quotient s1 2)) (string-reverse (substring s (quotient s1 2) s1)))
            (string=? (substring s 0 (quotient s1 2)) (string-reverse (substring s (+ (quotient s1 2) 1) s1))))))

(display (palindrome? "able was I ere I saw elba"))
(newline)
(display (palindrome? "mom n dad"))
(newline)
(display (palindrome? "haskell"))
(newline)

(exit)