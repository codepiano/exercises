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

(display '--------15.8)
(newline)

(define (stream-ref strm n)
    (cond ((stream-null? strm) the-null-stream)
          ((= n 0) (stream-car strm))
          (else (stream-ref (stream-cdr strm) (- n 1)))))

(define divides-by
    (lambda (n)
        (lambda (k)
        (zero? (remainder k n)))))

(define (stream-filter-out test?)
    (letrec ((helper (lambda (strm)
                        (let ((a (stream-car strm)))
                            (if (test? a)
                                (helper (stream-cdr strm))
                                (stream-cons a (helper (stream-cdr strm))))))))
        helper))

(define sieve (compose stream-filter-out divides-by))

(define positive-integers
    (stream-cons 1 (stream-map add1 positive-integers)))

(define prime-numbers
    (letrec ((primes (lambda (s)
                        (stream-cons (stream-car s)
                            (primes ((sieve (stream-car s)) (stream-cdr s)))))))
        (primes (stream-cdr positive-integers))))

(define has-prime-divisor?
    (lambda (n)
        (let ((max-value (sqrt n)))
            (letrec
                ((try (lambda (primes)
                    (and (<= (stream-car primes) max-value)
                         (or (zero? (remainder n (stream-car primes)))
                             (try (stream-cdr primes)))))))
                (try odd-primes)))))

(define odd-primes-builder
    (lambda (n)
        (if (has-prime-divisor? n)
            (odd-primes-builder (+ n 2))
            (stream-cons n (odd-primes-builder (+ n 2))))))

(define odd-primes (stream-cons 3 (odd-primes-builder 5)))

(define prime-numbers-another (stream-cons 2 odd-primes))

; 15.26
; (writeln (stream-ref prime-numbers 500))
; 15.27 fast
; (writeln (stream-ref prime-numbers-another 500))

(display '--------15.9)
(newline)

(define (stream-member? a strm n)
    (cond ((= n 0) #f)
           ((stream-null? strm) #f)
           ((= a (stream-car strm)) #t)
           (else (stream-member? a (stream-cdr strm) (- n 1)))))

(writeln (stream-member? 10 prime-numbers 20))
(writeln (stream-member? 19 prime-numbers 20))
(writeln (stream-member? 19 prime-numbers 5))

(display '--------15.10)
(newline)

(define (prime? a)
    (cond ((or (= a 2) (= a 3)) #t)
          ((has-prime-divisor? a) #f)
          (else (not (= 0 (remainder a 2))))))

(writeln (prime? 37))
(writeln (prime? 35))
(writeln (prime? 51))
(writeln (prime? 100000007))

(display '--------15.13)
(newline)

(define (diagonal i)
    (letrec
        ((stream-builder
            (lambda (x)
                (cond ((= (car x) 0) the-null-stream)
                      (else (stream-cons x (stream-builder (list (- (car x) 1) (+ (cadr x) 1)))))))))
        (stream-builder (list i 1))))

(writeln (finite-stream->list (diagonal 4)))
(writeln (finite-stream->list (diagonal 5)))

(display '--------15.14)
(newline)

(define stream-append-incorrect
    (lambda (finite-stream stream)
        (cond
            ((stream-null? finite-stream) stream)
             (else (stream-cons (stream-car finite-stream)
                        (stream-append-incorrect (stream-cdr finite-stream) stream))))))

(define int-pairs-generator-incorrect (lambda (i)
    (stream-append-incorrect (diagonal i) (int-pairs-generator (add1 i)))))

; recursion forever
; (writeln (int-pairs-generator-incorrect 1))

(display '--------15.15)
(newline)

(define stream-append/delay
    (lambda (finite-stream stream)
        (cond
            ((stream-null? finite-stream) (force stream))
             (else (stream-cons (stream-car finite-stream)
                        (stream-append/delay (stream-cdr finite-stream) stream))))))

(define-syntax stream-append (syntax-rules ()
    ((stream-append finite-stream stream)
     (stream-append/delay finite-stream (delay stream)))))

(define int-pairs-generator (lambda (i)
    (stream-append (diagonal i) (int-pairs-generator (add1 i)))))

(define int-pairs (int-pairs-generator 1))

(writeln (stream->list int-pairs 20))

(exit)