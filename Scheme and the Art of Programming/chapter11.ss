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

(exit)