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
