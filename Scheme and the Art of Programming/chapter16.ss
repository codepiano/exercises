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

(display '--------16.14)
(newline)

(writeln (let ((r (lambda (continuation) (continuation 6))))
    (* (+ (call/cc r) 3) 8)))

(writeln (let ((r (lambda (continuation)
                    (+ 1000 (continuation 6)))))
    (* (+ (call/cc r) 3) 8)))

(writeln (let ((r (lambda (continuation) (+ 1000 6))))
    (* (+ (call/cc r) 3) 8)))

(writeln (let ((r (lambda (continuation)
                (if (zero? (random 2))
                    (+ 1000 6)
                    (continuation 6)))))
            (* (+ (call/cc r) 3) 8)))

(writeln (let ((r (lambda (continuation)
                    (if (zero? (random 2))
                        (+ 1000 6)
                        (continuation 6)))))
            (+ (* (+ (call/cc r) 3) 8)
               (* (+ (call/cc r) 3) 8))))

(writeln (let ((r (lambda (continuation)
                    (if (zero? (random 2))
                        (+ 1000 6)
                        6))))
            (+ (* (+ (call/cc r) 3) 8)
               (* (+ (call/cc r) 3) 8))))

(display '--------16.16)
(newline)

(define deep "any continuation")

(define map-sub1
    (lambda (ls)
        (if (null? ls)
            (let ((receiver (lambda (k)
                                (set! deep k)
                                '())))
                (call/cc receiver))
                (cons (sub1 (car ls)) (map-sub1 (cdr ls))))))

(writeln (cons 1000 (map-sub1 '())))
(writeln (cons 2000 (deep '(a b c))))
(writeln (cons 1000 (map-sub1 '(0))))
(writeln (cons 2000 (deep '(a b c))))
(writeln (cons 1000 (map-sub1 '(1 0))))
(writeln (cons 2000 (deep '(a b c))))
(writeln (cons 1000 (map-sub1 '(5 4 3 2 1 0))))
(writeln (cons 2000 (deep '(a b c))))

(display '--------16.17)
(newline)

(define *escape/thunk* "any continuation")

(define receiver-4
    (lambda (continuation)
        (set! *escape/thunk* continuation)
        (*escape/thunk* (lambda () (writeln "escaper is defined")))))

((call/cc receiver-4))
(*escape/thunk* (lambda () (writeln (add1 6))))

(define escaper
    (lambda (proc)
        (lambda args
            (*escape/thunk*
                (lambda ()
                    (apply proc args))))))

(define et (escaper (lambda (x) (x))))

(et (lambda () (writeln (add1 6))))

(display '--------16.19)
(newline)

(define reset "reset")

((call/cc (lambda (c)
            (set! reset (lambda () (c (lambda () (writeln "reset invoked")))))
            (c (lambda () ())))))

(cons 1 (reset))

(display '--------16.21)
(newline)

(writeln (let ((r (escaper
                    (lambda (proc)
                        (cons 'c (proc (cons 'd '())))))))
    (cons 'a (cons 'b (call/cc r)))))

(writeln (let ((r (escaper
                    (lambda (proc)
                        (cons 'c (cons 'd '()))))))
    (cons 'a (cons 'b (call/cc r)))))

(exit)