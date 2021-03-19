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
(exit)