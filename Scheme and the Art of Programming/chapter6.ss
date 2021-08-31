(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

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

(display '--------6.10)
(newline)

; copy from book
(define legal?
    (lambda (try legal-pl)
        (letrec ((good?
                    (lambda (new-pl up down)
                        (cond
                            ((null? new-pl) #t)
                            (else (let ((next-pos (car new-pl)))
                                (and
                                    (not (= next-pos try))
                                    (not (= next-pos up))
                                    (not (= next-pos down))
                                    (good? (cdr new-pl)
                                           (add1 up)
                                           (sub1 down)))))))))
                (good? legal-pl (add1 try) (sub1 try)))))

; copy from book and modify a little
(define (queen n)
    (letrec ((forward
                (lambda (try legal-pl)
                    (cond
                        ((zero? try) (backtrack legal-pl))
                        ((legal? try legal-pl)
                            (build-solution n (cons try legal-pl)))
                        (else (forward (sub1 try) legal-pl)))))
            (backtrack
                (lambda (legal-pl)
                (cond
                    ((null? legal-pl) '())
                    (else (forward (sub1 (car legal-pl)) (cdr legal-pl))))))
            (build-solution
                (lambda (n legal-pl)
                    (cond
                        ((= n (length legal-pl)) legal-pl)
                        (else (forward n legal-pl))))))
        (build-solution n '())))


(display (queen 3))
(newline)
(display (queen 4))
(newline)
(display (queen 5))
(newline)
(display (queen 6))
(newline)
(newline)

(display '--------6.11)
(newline)

(define (sequence-legal? n legal-pl)
    (letrec ((takeFrom (lambda (a b ll)
                        (take (drop ll a) b)))
             (good? (lambda (size l)
                (letrec ((len (length l)))
                        (if (> (* 2 size) len)
                            #t
                            (let ((alist (takeFrom (- len (* 2 size)) size l))
                                  (blist (takeFrom (- len size) size l)))
                                (if (equal? alist blist)
                                    #f
                                    (good? (add1 size) l))))))))
        (good? 1 (append legal-pl (list n)))))

(display (sequence-legal? 1 (list 1 2 3)))
(newline)
(display (sequence-legal? 3 (list 1 2 3)))
(newline)
(display (sequence-legal? 2 (list 1 2 1)))
(newline)
(display (sequence-legal? 2 (list 2)))
(newline)

; same as queen, but use different legal? function
(define (goodSequence n)
    (letrec ((forward
                (lambda (try legal-pl)
                    (cond
                        ((zero? try) (backtrack legal-pl))
                        ((sequence-legal? try legal-pl)
                            (build-solution n (append legal-pl (list try))))
                        (else (forward (sub1 try) legal-pl)))))
            (backtrack
                (lambda (legal-pl)
                (cond
                    ((null? legal-pl) '())
                    (else (forward (sub1 (car legal-pl)) (cdr legal-pl))))))
            (build-solution
                (lambda (n legal-pl)
                    (cond
                        ((= n (length legal-pl)) legal-pl)
                        (else (forward 3 legal-pl))))))
        (build-solution n '())))

(display (goodSequence 3))
(newline)
(display (goodSequence 5))
(newline)
(display (goodSequence 20))
(newline)


(display '--------6.12)
(newline)

(define solution? (lambda (legal-pl)
    (= (length legal-pl) 8)))

(define fresh-try 8)


; five per line
(define searcher
    (lambda (legal? solution? fresh-try)
        (letrec
            ((build-solution
                (lambda (legal-pl)
                    (cond
                        ((solution? legal-pl) legal-pl)
                        (else (forward fresh-try legal-pl)))))
            (forward
                (lambda (try legal-pl)
                    (cond
                        ((zero? try) (backtrack legal-pl))
                        ((legal? try legal-pl)
                            (build-solution (cons try legal-pl)))
                        (else (forward (sub1 try) legal-pl)))))
            (backtrack
                (lambda (legal-pl)
                    (cond
                        ((null? legal-pl) '())
                        (else (forward (sub1 (car legal-pl)) (cdr legal-pl))))))
            (build-all-solutions
                (lambda ()
                    (letrec
                        ((loop (lambda (sol n)
                            (if (= n 4)
                                (newline)
                                #f)
                            (cond
                                ((null? sol) '())
                                (else (begin 
                                        (display sol)
                                        (loop (backtrack sol) (modulo (add1 n) 5))))))))
                        (loop (build-solution '()) 4)))))
            (build-all-solutions))))

(searcher legal? (lambda (x) (= (length x) 7)) 7)

(display '--------6.14)
(newline)

(define searcher-without-length
    (lambda (legal? k fresh-try)
        (letrec
            ((build-solution
                (lambda (legal-pl size)
                    (cond
                        ((= size k) legal-pl)
                        (else (forward fresh-try legal-pl size)))))
            (forward
                (lambda (try legal-pl size)
                    (cond
                        ((zero? try) (backtrack legal-pl size))
                        ((legal? try legal-pl)
                            (build-solution (cons try legal-pl) (add1 size)))
                        (else (forward (sub1 try) legal-pl size)))))
            (backtrack
                (lambda (legal-pl size)
                    (cond
                        ((null? legal-pl) '())
                        (else (forward (sub1 (car legal-pl)) (cdr legal-pl) (sub1 size))))))
            (build-all-solutions
                (lambda ()
                    (letrec
                        ((loop (lambda (sol n)
                            (if (= n 4)
                                (newline)
                                #f)
                            (cond
                                ((null? sol) '())
                                (else (begin 
                                        (display sol)
                                        (loop (backtrack sol k) (modulo (add1 n) 5))))))))
                        (loop (build-solution '() 0) 4)))))
            (build-all-solutions))))

(searcher-without-length legal? 7 7)

(display '--------6.15)
(newline)

(define searcher-without-length-combined
    (lambda (legal? k fresh-try)
        (letrec
            ((build-solution
                (lambda (try legal-pl size)
                    (cond
                        ((zero? try) 
                            (cond
                                ((null? legal-pl) '())
                                (else (build-solution (sub1 (car legal-pl)) (cdr legal-pl) (sub1 size)))))
                        ((= size k) legal-pl)
                        ((legal? try legal-pl)
                            (build-solution fresh-try (cons try legal-pl) (add1 size)))
                        (else (build-solution (sub1 try) legal-pl size)))))
            (build-all-solutions
                (lambda ()
                    (letrec
                        ((loop (lambda (sol n)
                            (if (= n 4)
                                (newline)
                                #f)
                            (cond
                                ((null? sol) '())
                                (else (begin 
                                        (display sol)
                                        (loop (build-solution 0 sol k) (modulo (add1 n) 5))))))))
                        (loop (build-solution fresh-try '() 0) 4)))))
            (build-all-solutions))))

(searcher-without-length-combined legal? 7 7)

(exit)