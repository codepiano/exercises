(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define (atom? x) (not (pair? x)))

(display '--------7.2)
(newline)

(define compose (lambda (f g)
    (lambda (x)
        (f (g x)))))

(define (compose3 g h x)
    (compose g (compose h x)))

(display ((compose3 add1 add1 add1) 2))
(newline)
(display ((compose3 add1 sub1 add1) 2))
(newline)

(display '--------7.3)
(newline)

(define compose-many (lambda args
    (if (null? args)
        (lambda (x) x)
        (compose (car args) (apply compose-many (cdr args))))))

(display ((compose-many add1 add1 add1 add1) 3))
(newline)
(display ( (compose-many sqrt abs sub1 (lambda (n) (* n n))) 0.6))
(newline)
(let ((f (lambda (n) (if (even? n) (/ n 2) (add1 n)))))
    (display ((compose-many f f f f f f) 21)))
(newline)


(display '--------7.4)
(newline)

(define substract (lambda (x y)
        (if (zero? y)
            x
            (sub1 (substract x (sub1 y))))))

(display (substract 4 3))
(newline)
(display (substract 100 99))
(newline)


(display '--------7.6)
(newline)

(define (map-first-two proc l)
    (letrec ((a (car l))
             (b (cdr l)))
        (if (or (null? l) (null? b))
            '()
            (cons (proc a (car b)) (map-first-two proc (cdr l))))))

(display (map-first-two + '(2 3 4 5 7)))
(newline)
(display (map-first-two max '(2 4 3 5 4 1)))
(newline)

(display '--------7.7)
(newline)

(define (reduce proc ls)
    (if (null? (cdr ls))
        (car ls)
        (reduce proc (cons (proc (car ls) (cadr ls)) (cddr ls)))))

(display (reduce + '(1 3 5 7 9)))
(newline)
(display (reduce max '(2 -4 6 8 3 1)))
(newline)
(display (reduce (lambda (x y) (and x y)) ' (#t #t #t #t)))
(newline)

(display '--------7.8)
(newline)

(define (andmap pred ls)
    (if (null? ls)
        #t
        (if (pred (car ls))
            (andmap pred (cdr ls))
            #f)))

(display (andmap positive? '(3 4 6 9)))
(newline)
(display (andmap positive? '(3 -1 4 8)))
(newline)
(let ((not-null? (compose not null?)))
    (display (andmap not-null? '((a b) (c) (c d e)))))
(newline)

(display '--------7.9)
(newline)

(define (map2 proc l1 l2)
    (if (null? l1)
        '()
        (cons (proc (car l1) (car l2)) (map2 proc (cdr l1) (cdr l2)))))

(display (map2 + '(1 2 3 4) '(5 7 9 11)))
(newline)
(display (map2 (let ((n 5))
                    (lambda (x y)
                    (and (< x n) (< n y)))) '(1 3 2 1 7) '(9 11 4 7 8)))
(newline)

(display '--------7.10)
(newline)

(define ormap
    (lambda (pred ls)
        (if (null? ls)
            #f
            (or (pred (car ls)) (ormap pred (cdr ls))))))

(define (any-null? l)
    (if (null? l)
        #f
        (ormap null? l)))

(display (any-null? (list '() '())))
(newline)
(display (any-null? (list 1 2)))
(newline)
(display (any-null? (list 1 2 '())))
(newline)
(newline)

(define map
    (lambda args
        (let ((proc (car args)))
            (letrec ((map 
                        (lambda (proc ls)
                            (if (null? ls)
                                '()
                                (cons (proc (car ls)) (map proc (cdr ls))))))
                     (map-helper
                        (lambda (a*)
                            (if (any-null? a*)
                                '()
                                (cons (apply proc (map car a*))
                                (map-helper (map cdr a*)))))))
                (map-helper (cdr args))))))

(display (map + '(1 2 3 4 5) '(2 3 4 5 6) '(4 5 6 7 8)))
(newline)

(display '--------7.12)
(newline)

(define (curried* x)
    (lambda (y)
        (* x y)))

(define (times10)
    (curried* 10))

(display ((times10) 1))
(newline)
(display ((times10) 10))
(newline)
(display ((times10) 0))
(newline)

(display '--------7.13)
(newline)

(define swapper-m 
    (lambda (x y)
        (letrec
            ((helper
                (lambda (ls)
                    (cond
                        ((null? ls) '())
                        ((equal? (car ls) x) (cons y (helper (cdr ls))))
                        ((equal? (car ls) y) (cons X (helper (cdr ls))))
                        (else (cons (car ls) (helper (cdr ls))))))))
            helper)))

(define (swapper-c x)
    (lambda (y)
        (swapper-m x y)))

(display (((swapper-c 0) 1) '(0 1 0 1 0 1 2 3)))
(newline)
(newline)

(display '--------7.14)
(newline)

(define (round-n-places n)
    (lambda (dec-num)
        (let ((scale-factor (expt 10 n)))
            (/ (round (* dec-num scale-factor)) scale-factor))))

(define round-5-places (round-n-places 5))

(display (round-5-places 0.123456789))
(newline)

(display '--------7.15)
(newline)

(define (subst-all-m new old)
    (letrec ((helper (lambda (ls)
                        (cond ((null? ls) '())
                              ((equal? (car ls) old) (cons new (helper (cdr ls))))
                              ((atom? (car ls)) (cons (car ls) (helper (cdr ls))))
                              (else (cons (helper (car ls)) (helper (cdr ls))))))))
        helper))

(display ((subst-all-m 1 0)'(0 1 2 1 2)))
(newline)
(display ((subst-all-m 1 0) '(0 1 2 ((0 1 2)))))
(newline)

(display '--------7.16)
(newline)

(define extreme-value-c
    (lambda (pred)
        (lambda (x y)
            (if (pred x y)
            x
            y))))

(define rmax (extreme-value-c >))
(define rmin (extreme-value-c <))

(display (rmax 1 2))
(newline)
(display (rmax 2 1))
(newline)
(display (rmin 1 2))
(newline)
(display (rmin 2 1))
(newline)

(display '--------7.17)
(newline)

(define extreme-value-c-arbiitrarily 
    (lambda (pred)
        (letrec ((helper (lambda args
                    (cond ((null? args) #f)
                          ((null? (cdr args)) (car args))
                          (else (let ((a (car args))
                                      (b (cdr args)))
                                    (if (pred a (car b))
                                        (apply helper (cons a (cdr b)))
                                        (apply helper b))))))))
            helper)))

(display (apply (extreme-value-c-arbiitrarily >) '(2 3 4 0 4 9 4 5)))
(newline)

(display '--------7.18)
(newline)

(define (between? x y z)
    (and (< y z) (> y x)))

(display (between? 1 2 3))
(newline)
(display (between? 1 3 2))
(newline)

(define between?-c
    (lambda (x)
        (lambda (y)
            (lambda (z)
                (and (< y z) (> y x))))))

(display (((between?-c 1) 2) 3))
(newline)
(display (((between?-c 1) 3) 2))
(newline)


(display '--------7.18)
(newline)

(define is-divisible-by?
    (lambda (n)
        (lambda (k)
            (zero? (remainder n k)))))

(define (prime? n)
    (let ((sqt (sqrt n)))
        (if (and (odd? n) (> n 1))
            (letrec ((div (is-divisible-by? n))
                    (helper (lambda (x)
                                (if (<= x sqt)
                                    (and (not (div x)) (helper (+ x 1)))
                                    #t))))
                (helper 3))
            #f)))

(display (prime? 9))
(newline)
(display (prime? 19))
(newline)
(exit)