(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

(display '--------2.17)
(newline)

(define (last-pair l)
    (if (null? (cdr l))
        l
        (last-pair (cdr l))))

(writeln (last-pair (list 1 2 3 4)))

(display '--------2.18)
(newline)

(define (reverse l)
    (if (null? (cdr l))
        l
        (append (reverse (cdr l)) (list (car l)))))

(writeln (reverse (list 1 2 3 4)))

(display '--------2.19)
(newline)

(define first-denomiantion car)

(define except-first-denomiantion cdr)

(define no-more? null?)

(display '--------2.20)
(newline)

(define (same-parity a . l)
    (define (iter e x)
        (if (null? x)
            '()
            (cond ((and e (even? (car x))) (cons (car x) (iter e (cdr x))))
                  ((and e (odd? (car x))) (iter e (cdr x)))
                  ((and (not e) (even? (car x))) (iter e (cdr x)))
                  (else (cons (car x) (iter e (cdr x)))))))
    (if (even? a)
        (iter #t (cons a l))
        (iter #f (cons a l))))

(writeln (same-parity 1 2 3 4 5 6 7))

(writeln (same-parity 2 3 4 5 6 7))

(display '--------2.21)
(newline)

(define (square-list items)
    (if (null? items)
        '()
        (cons (square (car items)) (square-list (cdr items)))))

(writeln (square-list (list 1 2 3 4)))

(define (square-list-map items)
    (map square items))

(writeln (square-list-map (list 1 2 3 4)))

(display '--------2.23)
(newline)

(define (my-for-each p l)
    (if (null? l)
        '()
        ((lambda () (p (car l)) (my-for-each p (cdr l))))))

(my-for-each (lambda (x) (newline)
            (display x))
            (list 57 321 88))

(display '--------2.25)
(newline)

(writeln (cadr (caddr (list 1 3 (list 5 7) 9))))
(writeln (caar (list (list 7))))
(writeln (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))

(display '--------2.26)
(newline)

(define x (list 1 2 3))
(define y (list 4 5 6))

(writeln (append x y))
(writeln (cons x y))
(writeln (list x y))

(display '--------2.27)
(newline)

(define (deep-reverse l)
    (cond ((null? l) '())
          ((pair? (car l)) (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
          (else (append (deep-reverse (cdr l)) (list (car l))))))

(define x (list (list 1 2) (list 3 4)))
(writeln (reverse x))
(writeln (deep-reverse x))

(display '--------2.28)
(newline)

(define (fringe l)
    (cond ((null? l) '())
          ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
          (else (cons (car l) (fringe (cdr l))))))


(define x (list (list 1 2) (list 3 4)))
(writeln (fringe x))
(writeln (fringe (list x x)))

(display '--------2.29.a)
(newline)

(define (make-mobile left right) (list left right))

(define (make-branch length structure) (list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

(display '--------2.29.b)
(newline)

(define (total-weight x)
    (cond ((null? x) 0)
          ((not (pair? x)) x)
          (else (+ (total-weight (branch-structure (left-branch x))) (total-weight (branch-structure (right-branch x)))))))

 (define m1 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8))))) 

(writeln (total-weight m1))

(display '--------2.29.c)
(newline)

(define (banlanced? x)
    (if (null? x)
        #t
        (and (= (* (branch-length (left-branch x)) (total-weight (branch-structure (left-branch x))))
                (* (branch-length (right-branch x)) (total-weight (branch-structure (right-branch x)))))
             (banlanced? (branch-structure (left-branch x)))
             (banlanced? (branch-structure (left-branch x))))))

(define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4))))) 

(writeln (banlanced? m1))
(writeln (banlanced? m2))

(exit)