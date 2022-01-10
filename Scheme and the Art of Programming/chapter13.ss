(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define 1st car)
(define 2nd cadr)

(display '--------13.1)
(newline)

(define unif-rand-var-0-1 
    (let ((big 1000000))
        (lambda ()
            (/ (+ 1 (random big)) big))))

(define (toss n even odd)
    (if (< n 1)
        (writeln "even:" even "\nodd:" odd)
        (let ((v1 (unif-rand-var-0-1))
              (v2 (unif-rand-var-0-1)))
            (if (or (and (<= v2 0.5) (> v1 0.5)) (and (> v2 0.5) (<= v1 0.5)))
                (toss (- n 1) (+ odd 1) even)
                (toss (- n 1) odd (+ even 1))))))

(writeln (toss 1 0 0))
(writeln (toss 10 0 0))
(writeln (toss 100 0 0))
(writeln (toss 1000 0 0))

(display '--------13.2)
(newline)

(define (car-plate n win t)
    (letrec ((bet (lambda (numbers a)
                    (if (< a 0)
                        #f
                        (let ((car-plate-number (random 100)))
                        (if (member car-plate-number numbers)
                            (begin
                                (writeln numbers "---------" car-plate-number)
                                #t)
                            (bet (cons car-plate-number numbers) (- a 1))))))))
        (if (< n 1)
            (writeln t "times bet, probability to win:" (/ win t))
            (if (bet '() 20)
                (car-plate (- n 1) (+ win 1) t)
                (car-plate (- n 1) win t)))))

(writeln (car-plate 100 0 100))

(display '--------13.3)
(newline)

(define (random-test n k)
    (let ((count (make-list k 0)))
        (letrec ((test (lambda (i)
                        (if (< i 1)
                            (writeln count)
                            (let* ((r (random 99))
                                   (index (modulo r k))
                                   (c (list-ref count index)))
                                (begin 
                                    (set-car! (list-tail count index) (+ c 1))
                                    (test (- i 1))))))))
            (test n))))

(writeln (random-test 50 7))
(exit)