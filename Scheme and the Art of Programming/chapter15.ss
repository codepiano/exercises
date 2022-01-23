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

(display '--------15.5)
(newline)

(define stream-car car)

(define stream-cdr (compose force cdr))

(define the-end-of-stream-tag "end of stream")

(define-syntax stream-cons (syntax-rules ()
    ((stream-cons expr stream)
     (cons expr (delay stream)))))

(define the-null-stream
    (stream-cons the-end-of-stream-tag the-null-stream))

(define list->stream
    (lambda (ls)
        (if (null? ls)
            the-null-stream
            (stream-cons (car ls) (list->stream (cdr ls))))))

(define end-of-stream?
    (lambda (x)
        (eq? x the-end-of-stream-tag)))

(define stream-null? (compose end-of-stream? stream-car))

(define stream->list
    (lambda (strm n)
        (if (or (zero? n) (stream-null? strm))
            '()
            (cons (stream-car strm) (stream->list (stream-cdr strm) (sub1 n))))))

(define finite-stream->list 
    (lambda (finite-strm)
        (stream->list finite-strm -1)))

(define build-stream
    (lambda (seed proc)
        (letrec
            ((stream-builder
                (lambda (x)
                    (stream-cons x (stream-builder (proc x))))))
            (stream-builder seed))))


(define stream-map
    (lambda (proc strm)
        (if (stream-null? strm)
            the-null-stream
            (stream-cons
                (proc (stream-car strm))
                (stream-map proc (stream-cdr strm))))))

(define (integers-from m)
    (build-stream m add1))

(writeln (stream->list (integers-from 6) 20))

(define (multiples-of k)
    (stream-map (lambda (n) (* n k)) (integers-from 0)))

(writeln (stream->list (multiples-of 6) 20))

(define (squares-of-integers)
    (stream-map (lambda (n) (* n n)) (integers-from 0)))

(writeln (stream->list (squares-of-integers) 20))

(display '--------15.6)
(newline)

(define all-integers 
    (let ((r (lambda (n)
                    (cond ((> n 0) (- 0 n))
                          ((<= n 0) (+ (- 0 n) 1))))))
        (build-stream 0 r)))

(writeln (stream->list all-integers 20))

(display '--------15.7)
(newline)

(define (stream-filter-in test?)
    (letrec ((helper (lambda (strm)
                        (let ((a (stream-car strm)))
                            (if (test? a)
                                (stream-cons a (helper (stream-cdr strm)))
                                (helper (stream-cdr strm)))))))
        helper))

(define odd-multiples-of-3
    ((stream-filter-in (lambda (x) (= 0 (remainder x 3)))) (integers-from 1)))

(writeln (stream->list odd-multiples-of-3 20))

(exit)