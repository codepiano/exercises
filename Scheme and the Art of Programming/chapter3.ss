; 3.1
(display '--------3.1)
(newline)

(define (sum data)
    (if (null? data)
        0
        (+ (car data) (sum (cdr data)))))

(display (sum '(1 2 3 4 5)))
(newline)
(display (sum '(6)))
(newline)
(display (sum '()))
(newline)
(newline)

; 3.2
(display '--------3.2)
(newline)

(define (pairwise-sum ntpl-1 ntpl-2)
    (if (null? ntpl-1)
        '()
        (cons (+ (car ntpl-1) (car ntpl-2)) (pairwise-sum (cdr ntpl-1) (cdr ntpl-2)))))

(display (pairwise-sum '(1 3 2) '(4 -1 2)))
(newline)
(display (pairwise-sum '(3.2 1.5) '(6.0 -2.5)))
(newline)
(display (pairwise-sum '(7) '(11)))
(newline)
(display (pairwise-sum '() '()))
(newline)
(newline)

(define (pairwise-product ntpl-1 ntpl-2)
    (if (null? ntpl-1)
        '()
        (cons (* (car ntpl-1) (car ntpl-2)) (pairwise-product (cdr ntpl-1) (cdr ntpl-2)))))

(display (pairwise-product '(1 3 2) '(4 -1 2)))
(newline)
(display (pairwise-product '(3.2 1.5) '(6.0 -2.5)))
(newline)
(display (pairwise-product '(7) '(11)))
(newline)
(display (pairwise-product '() '()))
(newline)
(newline)

; 3.3
(display '--------3.3)
(newline)

(define (dot-product l1 l2)
    (if (null? l1)
        0
        (sum (pairwise-product l1 l2))))

(display (dot-product '(3 4 -1) '(1 -2 -3)))
(newline)
(display (dot-product '(0.003 0.035) '(8 2)))
(newline)
(display (dot-product '(5.3e4) '(2.0e-3)))
(newline)
(display (dot-product '() '()) )
(newline)
(newline)


; 3.4
(display '--------3.4)
(newline)

(define (mult-by-n n ntpl)
    (if (null? ntpl)
        '()
        (cons (* n (car ntpl)) (mult-by-n n (cdr ntpl)))))

(display (mult-by-n 3 ' (1 2 3 4 5)))
(newline)
(display (mult-by-n 0 '(1 3 5 7 9 11)))
(newline)
(display (mult-by-n -7 '()))
(newline)
(newline)

; 3.5
(display '--------3.5)
(newline)

(define (index n data)
    (cond ((null? data) -1)
          ((eqv? n (car data)) 0)
          ((eqv? -1 (index n (cdr data))) -1)
          ((+ 1 (index n (cdr data))))))

(display (index 3 '(1 2 3 4 5 6)))
(newline)
(display (index 'so '(do re me fa so la ti do)))
(newline)
(display (index 'a '(b c d e)))
(newline)
(display (index 'cat '()))
(newline)
(newline)

; 3.6
(display '--------3.6)
(newline)

(define (make-list num a)
    (cond ((= num 0) '())
          (else (cons a (make-list (- num 1) a)))))

(define (all-same? data)
    (or (null? data)
        (null? (cdr data))
        (and (equal? (car data) (cadr data)) (all-same? (cdr data)))))

(display (make-list 5 'no))
(newline)
(display (make-list 1 'maybe))
(newline)
(display (make-list 0 'yes))
(newline)
(display (length (make-list 7 'any)))
(newline)
(display (all-same? (make-list 100 'any)) )
(newline)
(newline)

; 3.7
(display '--------3.7)
(newline)

(define (count-background a ls)
    (cond ((null? ls) 0)
          ((eqv? a (car ls)) (count-background a (cdr ls)))
          (else (+ 1 (count-background a (cdr ls))))))

(display (count-background 'blue '(red white blue yellow blue red)))
(newline)
(display (count-background 'red '(white blue green)))
(newline)
(display (count-background 'white '()))
(newline)
(newline)

; 3.8
(display '--------3.8)
(newline)

(define error
    (lambda args
        (display "Error:")
        (for-each (lambda (value) (display " ") (display value)) args)
        (newline)))

(define (list-front ls num)
    (cond ((> num (length ls)) (error "length of " ls "is less than " num))
          ((= num 0) '())
          ((cons (car ls) (list-front (cdr ls) (- num 1))))))

(display (list-front '(a b c d e f g) 4))
(newline)
(display (list-front '(a b c) 4))
(newline)
(display (list-front '(a b c d e f g) 0))
(newline)
(display (list-front '() 3))
(newline)
(newline)

; 3.9
(display '--------3.9)
(newline)

(define (wrapa a num)
    (cond ((= num 0) a)
          ((cons (wrapa a (- num 1)) '()))))

(display (wrapa 'gift 1))
(newline)
(display (wrapa 'semdwich 2))
(newline)
(display (wrapa 'prisoner 5))
(newline)
(display (wrapa 'moon 0))
(newline)
(newline)

; 3.10
(display '--------3.10)
(newline)

(define (multiple? m n)
    (and (not (= n 0)) (= 0 (remainder m n))))

(display (multiple? 7 2))
(newline)
(display (multiple? 9 3))
(newline)
(display (multiple? 5 0))
(newline)
(display (multiple? 0 20))
(newline)
(display (multiple? 17 1))
(newline)
(display (multiple? 0 0))
(newline)
(newline)

; 3.11
(display '--------3.11)
(newline)

(define (sum-of-odds n)
    (cond ((= n 0) 0)
          ((= 0 (remainder n 2)) (sum-of-odds (sub1 n)))
          (else (+ n (sum-of-odds (sub1 n))))))

(display (sum-of-odds 10))
(newline)
(display (sum-of-odds 9))
(newline)
(display (sum-of-odds 8))
(newline)
(display (sum-of-odds 7))
(newline)
(display (sum-of-odds 6))
(newline)
(display (sum-of-odds 5))
(newline)
(display (sum-of-odds 3))
(newline)
(display (sum-of-odds 1))
(newline)
(newline)

; 3.12
(display '--------3.12)
(newline)

(define (n-tuple->integer ls)
    (cond ((null? ls) (error "bad argement () to n-tuple->integer"))
           (else (n-tuple->integer-helper ls))))

(define (n-tuple->integer-helper ls)
    (cond ((null? ls) 0)
          (else (+ (* (car ls) (expt 10 (length (cdr ls)))) (n-tuple->integer-helper (cdr ls))))))

(display (n-tuple->integer '(3 1 4 6)))
(newline)
(display (n-tuple->integer '(0)))
(newline)
(display (n-tuple->integer '()))
(newline)
(display (+ (n-tuple->integer '(1 2 3)) (n-tuple->integer '(3 2 1))))
(newline)
(newline)

; 3.14
(display '--------3.14)
(newline)

(define numr
    (lambda (rtl)
        (car rtl)))

(define denr
    (lambda (rtl)
        (cadr rtl)))

(define make-ratl
    (lambda (intl int2)
        (if (zero? int2)
            (error "make-ratl: The denominator cannot be zero.")
            (list intl int2))))

(define (rminus rnum)
    (make-ratl (- 0 (numr rnum)) (denr rnum)))

(display (rminus (make-ratl 3 5)))
(newline)
(newline)

; 3.15
(display '--------3.15)
(newline)

(define (same-sign? a b)
    (or (and (>= a 0) (>= b 0))
        (and (< a 0) (< b 0))))

(display (same-sign? 1 2))
(newline)
(display (same-sign? 1 -2))
(newline)
(display (same-sign? -1 -2))
(newline)
(display (same-sign? -1 0))
(newline)
(newline)

; 3.16
(display '--------3.16)
(newline)

(define (rabs rnum)
    (make-ratl (abs (numr rnum)) (abs (denr rnum))))

(display (rabs (make-ratl -1 2)))
(newline)
(display (rabs (make-ratl -1 -2)))
(newline)
(display (rabs (make-ratl 1 2)))
(newline)
(newline)

; 3.17
(display '--------3.17)
(newline)

(define (make-ratl-low a b)
    (if (< b 0)
        (make-ratl-low-helper (* -1 a) (abs b))
        (make-ratl-low-helper a b)))

(define (make-ratl-low-helper a b)
    (cond ((= (gcd a b) 1) (make-ratl a b))
          (else (make-ratl-low-helper (/ a (gcd a b))
                                      (/ b (gcd a b))))))
                                    
(display (make-ratl-low 24 30))
(newline)
(display (make-ratl-low -10 15))
(newline)
(display (make-ratl-low 8 -10))
(newline)
(display (make-ratl-low -6 -9))
(newline)
(display (make-ratl-low 0 8))
(newline)
(newline)

(exit)