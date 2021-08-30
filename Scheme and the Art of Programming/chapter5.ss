(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

; 5.6
(display '--------5.6)
(newline)

(define (insert-left-all new old ls)
    (letrec ((insert-left 
                (lambda (ls) 
                    (cond ((null? ls) '())
                          ((equal? old (car ls)) (cons new (cons old (insert-left (cdr ls)))))
                          ((pair? (car ls)) (cons (insert-left (car ls)) (insert-left (cdr ls))))
                          (else (cons (car ls) (insert-left (cdr ls))))))))
        (insert-left ls)))

(display (insert-left-all 'z 'a '(a ((b a) ((a (c)))))))
(newline)
(display (insert-left-all 'z 'a '(((a)))))
(newline)
(display (insert-left-all 'z 'a '()))
(newline)
(newline)

; 5.7
(display '--------5.7)
(newline)

(define (fib n)
    (letrec ((fib-it (lambda (n acc1 acc2)
                        (if (= n 1)
                            acc2
                            (fib-it (sub1 n) acc2 (+ acc1 acc2))))))
        (fib-it n 0 1)))

(display (fib 10))
(newline)
(display (fib 50))
(newline)

; 5.8
(display '--------5.8)
(newline)

(define error
    (lambda args
        (display "Error:")
        (for-each (lambda (value) (display " ") (display value)) args)
        (newline)))

(define list-ref
    (lambda (l a)
        (letrec ((list-ref-inner
                    (lambda (ls n)
                        (cond ((null? ls) (error "list-ref: Index" a "out of range for list" l))
                              ((zero? n) (car ls))
                              (else (list-ref-inner (cdr ls) (sub1 n)))))))
            (list-ref-inner l a))))

(display (list-ref '(1 2 3 4 5) 3))
(newline)
(display (list-ref '() 3))
(newline)
(display (list-ref '(1 2) 3))
(newline)
(newline)

; 5.10
(display '--------5.10)
(newline)

(define make-term
    (lambda (deg coef)
        (poly-cons deg coef the-zero-poly)))

(define leading-term
    (lambda (poly)
        (make-term (degree poly) (leading-coef poly))))

(define zero-poly?
    (lambda (poly)
        (and (zero? (degree poly)) (zero? (leading-coef poly)))))

(define the-zero-poly '((0 0)))

(define degree
    (lambda (poly)
        (caar poly)))

(define leading-coef
    (lambda (poly)
        (cadar poly)))

(define rest-of-poly
    (lambda (poly)
        (if (null? (cdr poly))
            the-zero-poly
            (cdr poly))))

(define poly-cons
    (lambda (deg coef poly)
        (let ((deg-p (degree poly)))
            (cond ((and (zero? deg) (equal? poly the-zero-poly)) (list (list 0 coef)))
                  ((>= deg-p deg) (error "poly-cons: Degree too high in" poly))
                  ((zero? coef) poly)
                  (else (cons (list deg coef) poly))))))

(define p+
    (lambda (poly1 poly2)
        (cond ((zero-poly? poly1) poly2)
            ((zero-poly? poly2) poly1)
            (else (let ((n1 (degree poly1))
                        (n2 (degree poly2)))
                        (cond ((> n1 n2) (let ((rest1 (rest-of-poly poly1))
                                               (a1 (leading-coef poly1)))
                                            (poly-cons n1 a1 (p+ rest1 poly2))))
                            ((< n1 n2) (let ((rest2 (rest-of-poly poly2))
                                             (a2 (leading-coef poly2)))
                                            (poly-cons n2 a2 (p+ poly1 rest2))))
                            (else (let ((a1 (leading-coef poly1))
                                        (a2 (leading-coef poly2))
                                        (rest1 (rest-of-poly poly1))
                                        (rest2 (rest-of-poly poly2)))
                                        (poly-cons n1 (+ a1 a2) (p+ rest1 rest2))))))))))

(define p1 (poly-cons 3 5 (poly-cons 2 -3 (poly-cons 1 1 (poly-cons 0 -17 the-zero-poly)))))
(define p2 (poly-cons 3 5 (poly-cons 2 -3 (poly-cons 1 1 (poly-cons 0 -17 the-zero-poly)))))
(display (p+ p1 p2))
(newline)
(newline)

; 5.11
(display '--------5.11)
(newline)

(define p*
    (letrec ((t* (lambda (trm poly)
        (if (zero-poly? poly)
            the-zero-poly
            (poly-cons (+ (degree trm) (degree poly))
                       (* (leading-coef trm) (leading-coef poly))
                       (t* trm (rest-of-poly poly)))))))
        (lambda (poly1 poly2)
            (letrec ((p*-helper (lambda (p1)
                                    (if (zero-poly? p1)
                                        the-zero-poly
                                        (p+ (t* (leading-term p1) poly2) (p*-helper (rest-of-poly p1)))))))
                (p*-helper poly1)))))

(define negative-poly
    (lambda (poly)
        (let ((poly-negative-one (make-term 0 -1)))
            (p* poly-negative-one poly))))

(define p-
    (lambda (poly1 poly2)
        (p+ poly1 (negative-poly poly2))))

(define (poly-quotient p1 p2)
    (cond ((zero-poly? p1) the-zero-poly)
          ((zero-poly? p2) (error "divisor is zero poly"))
          (else (let ((dg1 (degree p1))
                      (dg2 (degree p2)))
                      (cond ((> dg2 dg1) the-zero-poly)
                            (else (let ((d1 (/ (leading-coef p1) (leading-coef p2)))
                                        (d2 (- dg1 dg2)))
                                  (poly-cons d2 d1 (poly-quotient (p- p1 (p* p2 (poly-cons d2 d1 the-zero-poly))) p2)))))))))

(define p3 (poly-cons 3 1 (poly-cons 2 -3 (poly-cons 1 -1 (poly-cons 0 -1 the-zero-poly)))))
(define p4 (poly-cons 2 3 (poly-cons 1 -2 (poly-cons 0 1 the-zero-poly))))

(display (poly-quotient p3 the-zero-poly))
(newline)
(display (poly-quotient p3 p4))
(newline)
(newline)

(define (poly-remainder p1 p2)
    (cond ((zero-poly? p1) the-zero-poly)
          ((zero-poly? p2) (error "divisor is zero poly"))
          (else (let ((dg1 (degree p1))
                      (dg2 (degree p2)))
                      (cond ((> dg2 dg1) p1)
                            (else (let ((d1 (/ (leading-coef p1) (leading-coef p2)))
                                        (d2 (- dg1 dg2)))
                                  (poly-remainder (p- p1 (p* p2 (poly-cons d2 d1 the-zero-poly))) p2))))))))

(display (poly-remainder p3 the-zero-poly))
(newline)
(display (poly-remainder p3 p4))
(newline)
(newline)

(exit)