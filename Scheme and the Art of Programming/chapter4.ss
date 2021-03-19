; 4.1
(display '--------4.1)
(newline)

(define (insert-left new old ls)
      (cond ((null? ls) '())
            ((equal? old (car ls)) (cons new (cons old (insert-left new old (cdr ls)))))
            (else (cons (car ls) (insert-left new old (cdr ls))))))

(display (insert-left 'z 'a '(a b a c a)))
(newline)
(display (insert-left 0 1 '(0 1 0 1)))
(newline)
(display (insert-left 'dog 'cat '(my dog is fun)))
(newline)
(display (insert-left 'two 'one '()))
(newline)
(newline)

; 4.2
(display '--------4.2)
(newline)

(define (insert-right new old ls)
      (cond ((null? ls) '())
            ((equal? old (car ls)) (cons old (cons new (insert-right new old (cdr ls)))))
            (else (cons (car ls) (insert-right new old (cdr ls))))))

(display (insert-right 'z 'a '(a b a c a)))
(newline)
(display (insert-right 0 1 '(0 1 0 1)))
(newline)
(display (insert-right 'dog 'cat '(my dog is fun)))
(newline)
(display (insert-right 'two 'one '()))
(newline)
(newline)

; 4.3
(display '--------4.3)
(newline)

(define (subst new old ls)
      (cond ((null? ls) '())
            ((equal? old (car ls)) (cons new (subst new old (cdr ls))))
            (else (cons (car ls) (subst new old (cdr ls))))))

(display (subst 'z 'a '(a b a c a)))
(newline)
(display (subst 0 1 '(0 1 0 1)))
(newline)
(display (subst 'dog 'cat '(my dog is fun)))
(newline)
(display (subst 'two 'one '()))
(newline)
(newline)

; 4.4
(display '--------4.4)
(newline)

(define (deepen-1 ls)
      (if (null? ls) 
          '()
          (cons (cons (car ls) '()) (deepen-1 (cdr ls)))))

(display (deepen-1 '(a b e d)))
(newline)
(display (deepen-1 '((a b) (c (d e)) f)))
(newline)
(display (deepen-1 '()))
(newline)
(newline)

; 4.5
(display '--------4.5)
(newline)

(define (subst-all new old ls)
      (cond ((null? ls) '())
            ((equal? (car ls) old) (cons new (subst-all new old (cdr ls))))
            ((atom? (car ls)) (cons (car ls) (subst-all new old (cdr ls))))
            (else (cons (subst-all new old (car ls)) (subst-all new old (cdr ls))))))

(display (subst-all 'z 'a '(a (b (a c)) (a (d a)))))
(newline)
(display (subst-all 0 '(1) '(((1) (0)))))
(newline)
(display (subst-all 'one 'two '()))
(newline)
(newline)

; 4.6
(display '--------4.6)
(newline)

(define (insert-left-all new old ls)
      (cond ((null? ls) '())
            ((equal? old (car ls)) (cons new (cons old (insert-left-all new old (cdr ls)))))
            ((pair? (car ls)) (cons (insert-left-all new old (car ls)) (insert-left-all new old (cdr ls))))
            (else (cons (car ls) (insert-left-all new old (cdr ls))))))

(display (insert-left-all 'z 'a '(a ((b a) ((a (c)))))))
(newline)
(display (insert-left-all 'z 'a '(((a)))))
(newline)
(display (insert-left-all 'z 'a '()))
(newline)
(newline)

; 4.7
(display '--------4.7)
(newline)

(define (sum-all ls)
      (cond ((null? ls) 0)
            ((pair? (car ls)) (+ (sum-all (car ls)) (sum-all (cdr ls))))
            (else (+ (car ls) (sum-all (cdr ls))))))    

(display (sum-all '((1 3) (5 7) (9 11))))
(newline)
(display (sum-all '(1 (3 (5 (7 (9)))))))
(newline)
(display (sum-all '()))
(newline)
(newline)

; 4.8
(display '--------4.8)
(newline)

(define (count-parens-all ls)
      (cond ((null? ls) 2)
            ((null? (car ls)) (+ 2 (count-parens-all (cdr ls))))
            ((pair? (car ls)) (+ (count-parens-all (car ls)) (count-parens-all (cdr ls))))
            (else (count-parens-all (cdr ls)))))

(display (count-parens-all '()))
(newline)
(display (count-parens-all '((a b) c)))
(newline)
(display (count-parens-all '(((a () b) c) () ((d) e))))
(newline)
(newline)

; 4.9
(display '--------4.9)
(newline)

(define (count-background-all item ls)
      (cond ((null? ls) 0)
            ((pair? (car ls)) (+ (count-background-all item (car ls)) (count-background-all item (cdr ls))))
            ((equal? item (car ls)) (count-background-all item (cdr ls)))
            (else (+ 1 (count-background-all item (cdr ls))))))

(display (count-background-all 'a '((a) b (c a) d)))
(newline)
(display (count-background-all 'a ' ( ( ( (b (((a)) c)))))))
(newline)
(display (count-background-all 'b '()))
(newline)
(newline)

; 4.10
(display '--------4.10)
(newline)

(define (leftmost ls)
      (cond ((null? ls) '())
            ((pair? (car ls)) (leftmost (car ls)))
            (else (car ls))))

(display (leftmost '((a b) (c (d e)))))
(newline)
(display (leftmost '((((c ((e f) g) h))))))
(newline)
(display (leftmost '(() a)))
(newline)
(newline)

; 4.11
(display '--------4.11)
(newline)

(define (rightmost ls)
      (cond ((null? ls) '())
            ((null? (cdr ls)) (if (pair? (car ls))
                                 (rightmost (car ls))
                                 (car ls)))
            (else (rightmost (cdr ls)))))

(display (rightmost '((a b) (d (c d (f (g h) i) m n) u) v)))
(newline)
(display (rightmost '((((((b (c)))))))))
(newline)
(display (rightmost '(a ())) )
(newline)
(newline)

; 4.14
(display '--------4.14)
(newline)

(define (harmonic-sum-it n result)
      (if (= n 0) 
          result
          (harmonic-sum-it (- n 1) (+ result (/ 1 n)))))

(display (harmonic-sum-it 10 0))
(newline)
(display (harmonic-sum-it 100 0))
(newline)
(newline)

; 4.17
(display '--------4.17)
(newline)

(define (calls-fib n)
      (cond ((= n 0) 1)
            ((= n 1) 1)
            (else (add1 (+ (calls-fib (- n 1)) (calls-fib (- n 2)))))))

(display (calls-fib 10))
(newline)
(display (calls-fib 30))
(newline)
(newline)

(define (adds-fib n)
      (cond ((= n 0) 0)
            ((= n 1) 0)
            (else (add1 (+ (adds-fib (- n 1)) (adds-fib (- n 2)))))))

(display (adds-fib 10))
(newline)
(display (adds-fib 30))
(newline)
(newline)

; 4.18
(display '--------4.18)
(newline)

(define (length-it ls n)
      (cond ((null? ls) n)
            (else (length-it (cdr ls) (+ n 1)))))

(display (length-it '(1 2 3 4 5) 0))
(newline)
(display (length-it '() 0))
(newline)
(display (length-it '(1) 0))
(newline)

; 4.19
(display '--------4.19)
(newline)

(define (mk-asc-list-of-ints n ls)
      (if (= n 0)
          ls
          (mk-asc-list-of-ints (- n 1) (cons n ls))))

(display (mk-asc-list-of-ints 10 '()))
(newline)
(newline)

(define (mk-desc-list-of-ints n ls)
      (if (= n 0)
          ls
          (mk-desc-list-of-ints (- n 1) (append ls (cons n '())))))

(display (mk-desc-list-of-ints 10 '()))
(newline)
(newline)

(exit)