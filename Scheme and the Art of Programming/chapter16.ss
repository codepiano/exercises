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


(display '--------16.12)
(newline)

(define writeln/return
    (lambda (x)
        (writeln x)
        x))


(define call
    (lambda (receiver)
        (receiver writeln/return)))


(define answer-maker
    (lambda (x)
        (call (lambda (r) (cons 'answer-is (r x))))))

(writeln (answer-maker 42))
(exit)
