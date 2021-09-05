(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(display '--------11.2)
(newline)

(define (pascal-triangle n k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          (else (+ (pascal-triangle (sub1 n) (sub1 k)) (pascal-triangle (sub1 n) k)))))


;(writeln (pascal-triangle 10 5))
;(writeln (pascal-triangle 12 6))
;(writeln (pascal-triangle 14 7))
;(writeln (pascal-triangle 16 8))

(display '--------11.3)
(newline)

(define time-of-day get-universal-time)

(define timer2
    (lambda (proc arg1 arg2)
        (let* ((start (time-of-day))
               (val (proc arg1 arg2))
               (finish (time-of-day))
               (elapsed-time (- finish start)))
            (writeln "Time = " elapsed-time ", Answer = " val))))

;(timer2 pascal-triangle 10 5)
;(timer2 pascal-triangle 12 6)
;(timer2 pascal-triangle 14 7)
;(timer2 pascal-triangle 20 8)
;(timer2 pascal-triangle 26 8)

(display '--------11.4)
(newline)

(define (fact n)
    (cond ((= n 1) 1)
          (else (* n (fact (sub1 n))))))

(define (combinations n k)
    (/ (fact n) (* (fact k) (fact (- n k)))))

(writeln (combinations 16 8))

(display '--------11.5)
(newline)

(define lookup
    (lambda (obj table success-proc failure-proc)
        (letrec ((lookup (lambda (table)
            (if (null? table)
                (failure-proc)
                    (let ((pr (car table)))
                        (if (equal? (car pr) obj)
                            (success-proc pr)
                            (lookup (cdr table))))))))
            (lookup table))))

(define lookup2
    (lambda (obj1 obj2 table success-proc failure-proc)
        (letrec ((lookup (lambda (table)
            (if (null? table)
                (failure-proc)
                    (let ((pr (car table)))
                        (if (and (equal? (car pr) obj1) (equal? (cadr pr) obj2))
                            (success-proc pr)
                            (lookup (cdr table))))))))
            (lookup table))))

(writeln (lookup2 'a 'c '((a b 5) (a c 7) (b c 9)) (lambda (tr) tr) (lambda ()'())))
(writeln (lookup2 'a 'c '((a b 5) (c a 7) (b c 9)) (lambda (tr) tr) (lambda ()'())))

(display '--------11.6)
(newline)

(define memoize2
    (lambda (proc)
        (let ((table '()))
            (lambda (arg1 arg2)
                (lookup2 arg1 arg2 table
                    (lambda (pr) (caddr pr))
                    (lambda ()
                        (let ((val (proc arg1 arg2)))
                            (set! table (cons arg1 (cons arg2 (cons val table))))
                            val)))))))

(display '--------11.7)
(newline)

(define (memo-pascal-triangle  n k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          (else (+ (memo-pascal-triangle (sub1 n) (sub1 k)) (memo-pascal-triangle (sub1 n) k)))))


;(timer2 pascal-triangle 26 8)
;(timer2 memo-pascal-triangle 26 8)

(display '--------11.8)
(newline)

(define timer*
    (lambda args
        (let ((proc (car args)))
            (apply proc (cdr args)))))

(writeln (timer* pascal-triangle 4 2))

(display '--------11.12)
(newline)

(writeln (let ((x (list 1 2)))
    (let ((y (list 3)))
        (let ((z (cons x y)))
            (set-cdr! y x)
            (set-cdr! (cdr z) x)
            z))))

(display '--------11.13)
(newline)

(define a (let ((x (cons 1 1)))
    (set-car! x x)
    (set-cdr! x x)
    x))

(define (cycle? l)
    (letrec ((helper (lambda (x y)
                        (cond ((null? y) #f)
                              ((null? (cdr y)) #f)
                              ((eq? (car x) (car y)) #t)
                              (else (helper (cdr x) (cddr y)))))))
        (if (null? l)
            #f
            (helper l (cdr l)))))

(writeln (cycle? a))

(define (remove-cycle l)
    (if (null? l)
        '()
        (if (cycle? (car l))
            (remove-cycle (cdr l))
            (cons (car l) (remove-cycle (cdr l))))))

(writeln (remove-cycle (list '(1 2 3) a '(4 5 6) a)))

(display '--------11.15)
(newline)

(define x (list 'a 'b 'c 'd 'e))
(set-cdr! (last-pair x) (cdr (cdr x)))

(define last-pair
    (lambda (x)
        (if (pair? (cdr i))
            (last-pair (cdr x))
            x)))

(writeln (cycle? x))
(writeln (cycle? (list 1 2 3 4)))
(writeln (cycle? (list 1)))
(writeln (cycle? '()))
(newline)

(define writeln2 (lambda (x) 
    (if (not (cycle? x))
        (begin (display x)
                (newline))
        (writeln "param has cycle"))))

(writeln2 x)
(writeln2 '(1 2 3 4))

(display '--------11.20)
(newline)

(define reconfigure
    (lambda (tape character direction)
        (if (eq? direction 'left)
            (left (overwrite character tape))
            (right (overwrite character tape)))))

(define at 
    (lambda (tape)
        (let ((right-part (cadr tape)))
            (car right-part))))


(define overwrite
    (lambda (char tape)
        (let ((left-part (car tape))
              (right-part (cadr tape)))
              (let ((new-right-part (cons char (cdr right-part))))
                (list left-part new-right-part)))))

(define check-null
    (lambda (part)
        (if (null? part)
            (list 0)
            part)))

(define right
    (lambda (tape)
        (let ((left-part (car tape))
              (right-part (cadr tape)))
                (let ((new-left-part (cons (car right-part) left-part))
                      (new-right-part (cdr right-part)))
                      (list new-left-part (check-null new-right-part))))))

(define left
    (lambda (tape)
        (let ((left-part (car tape))
              (right-part (cadr tape)))
                (let ((new-right-part (cons (car left-part) right-part))
                      (new-left-part (cdr left-part)))
                      (list new-right-part (check-null new-left-part))))))

(define test-reconfigure
    (lambda ()
        (let ((tape1 (list (list 'a 'b 'c 0) (list 'x 'y 0))))
            (let ((tape2 (reconfigure tape1 'u 'right))
                  (tape3 (reconfigure tape1 'd 'left)))
                (let ((tape4 (reconfigure tape2 'v 'right))
                      (tape5 (reconfigure tape3 'e 'left)))
                    (let ((tape6 (reconfigure tape4 'w 'right))
                          (tape7 (reconfigure tape5 'f 'left)))
                        (let ((tape8 (reconfigure tape6 'x 'right))
                              (tape9 (reconfigure tape7 'g 'left)))
                            (list tape8 tape9))))))))

(writeln (test-reconfigure))

(display '--------11.21)
(newline)

(define (list->tape ls)
    (cons '(0) (list (append ls '(0)))))

(writeln (list->tape '(1 2)))

(define (tape->list t)
    (let ((l (car t))
          (r (cadr t)))
        (letrec ((left (cdr (reverse l)))
                 (remove-last (lambda (ls)
                                (if (null? (cdr ls))
                                    '()
                                    (cons (car ls) (remove-last (cdr ls)))))))
            (append left (remove-last r)))))

(writeln (tape->list (list->tape '(1 2 3 4))))

(exit)