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
    (if (or (null? x) (not (pair? x)))
        #t
        (and (= (* (branch-length (left-branch x)) (total-weight (branch-structure (left-branch x))))
                (* (branch-length (right-branch x)) (total-weight (branch-structure (right-branch x)))))
             (banlanced? (branch-structure (left-branch x)))
             (banlanced? (branch-structure (right-branch x))))))

(define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4))))) 

(writeln (banlanced? m1))
(writeln (banlanced? m2))

(display '--------2.30)
(newline)

(define (square-tree tree)
    (cond ((null? tree) tree)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(writeln (square-tree
       (list 1 (list 2 (list 3 4) 5) (list 6 7))))

(define (square-tree-map tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (square sub-tree))) tree))

(writeln (square-tree-map
       (list 1 (list 2 (list 3 4) 5) (list 6 7))))

(display '--------2.31)
(newline)

(define (tree-map proc tree)
    (cond ((null? tree) tree)
          ((not (pair? tree)) (proc tree))
          (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(define (new-square-tree tree) (tree-map square tree))

(writeln (new-square-tree
       (list 1 (list 2 (list 3 4) 5) (list 6 7))))


(display '--------2.32)
(newline)

(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(writeln (subsets (list 1 2 3)))

(display '--------2.33)
(newline)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (map p sequence) (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2) (accumulate cons seq1 seq2))
(define (length sequence) (accumulate + 0 sequence))

(display '--------2.34)
(newline)

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) 
                    (+ this-coeff (* x higher-terms))) 0 coefficient-sequence))

(writeln (horner-eval 2 (list 1 3 0 5 0 1)))

(display '--------2.35)
(newline)

(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) (if (pair? x)
                                         (count-leaves x)
                                         1)) t)))

 (writeln (count-leaves (list 1 2 (list 3 4) (list 5 (list 6 7)))))

(display '--------2.36)
(newline)

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(writeln (accumulate-n + 0 (list (list 1 2 3) (list 40 50 60) (list 700 800 900))))

(exit)