(define writeln (lambda x (for-each display x) (newline)))

(define 1st car)
(define 2nd cadr)

(define compose (lambda (f g)
    (lambda (x)
        (f (g x)))))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define the-null-delayed-list '())

(define delayed-list-null? null?)

(define delayed-list-car car)

(define delayed-list-cdr (compose force cdr))

(define-syntax delayed-list-cons (syntax-rules ()
    ((delayed-list-cons expr del-list)
     (cons expr (delay del-list)))))

(define delayed-list->list 
    (lambda (delayed-list)
        (if (delayed-list-null? delayed-list)
            '()
            (cons (delayed-list-car delayed-list) (delayed-list->list (delayed-list-cdr delayed-list))))))

(display '--------15.1)
(newline)

(define (first-n-even-delayed-list n)
    (if (zero? n)
        the-null-delayed-list
        (delayed-list-cons (* 2 (sub1 n)) (first-n-even-delayed-list (sub1 n)))))

(writeln (delayed-list->list (first-n-even-delayed-list 10)))


(define (first-n-odd-delayed-list n)
    (if (zero? n)
        the-null-delayed-list
        (delayed-list-cons (add1 (* 2 (sub1 n))) (first-n-odd-delayed-list (sub1 n)))))

(writeln (delayed-list->list (first-n-odd-delayed-list 10)))

(exit)