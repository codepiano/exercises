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

(display '--------15.2)
(newline)

(define (list->delayed-list list)
    (cond ((null? list) the-null-delayed-list)
          ((delayed-list-cons (car list) (list->delayed-list (cdr list))))))

(writeln (delayed-list->list (list->delayed-list '(1 2 3 4 5))))

(display '--------15.3)
(newline)

(define (delayed-list-sum k ls)
    (cond ((null? ls) 0)
          ((= k 0) 0)
          ((+ (delayed-list-car ls) (delayed-list-sum (- k 1) (delayed-list-cdr ls))))))

(writeln (delayed-list-sum 3 (list->delayed-list '(1 2 3 4 5))))

(define (delayed-list-product k ls)
    (cond ((null? ls) 1)
          ((= k 0) 1)
          ((* (delayed-list-car ls) (delayed-list-product (- k 1) (delayed-list-cdr ls))))))

(writeln (delayed-list-product 3 (list->delayed-list '(1 2 3 4 5))))

(define (delayed-list-accumulate k ls op)
    (cond ((null? ls) 1)
          ((= k 0) 1)
          ((op (delayed-list-car ls) (delay (delayed-list-accumulate (- k 1) (delayed-list-cdr ls) op))))))

(define (delayed-list-sum-acc k ls)
    (delayed-list-accumulate k ls (lambda (a expr) (+ a (force expr)))))

(define (delayed-list-product-acc k ls)
    (delayed-list-accumulate k ls (lambda (a expr)
                                        (if (= a 0)
                                            0
                                            (* a (force expr))))))

(writeln (delayed-list-product-acc 3 (list->delayed-list '(1 2 3 4 5))))
(writeln (delayed-list-product-acc 10 (list->delayed-list '(1 2 3 4 5 0 6 7 8 9))))

(exit)