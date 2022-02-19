(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

(display '--------3.1)
(newline)

(define (make-accumulator n)
    (let ((sum n))
        (lambda (x)
            (begin (set! sum (+ sum x))
                   sum))))

(define A (make-accumulator 5))
(writeln (A 10))
(writeln (A 10))

(display '--------3.2)
(newline)

(define (make-monitored f)
    (let ((count 0))
        (lambda (x)
            (cond ((eq? x 'how-many-calls?) count)
                  ((eq? x 'reset-count) (set! count 0))
                  (else (begin (set! count (+ count 1))
                               (f x)))))))
 
(define s (make-monitored sqrt))

(writeln (s 100))

(writeln (s 'how-many-calls?))

(display '--------3.3)
(newline)

(define (make-account balance passwd)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                    balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m pass)
        (if (eq? pass passwd)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT" m)))
            "Incorrect Password"))
    dispatch)

(define account (make-account 100 'right-password))

(writeln (account 'withdraw 'wrong-pass))

(writeln ((account 'withdraw 'right-password) 10))

(display '--------3.4)
(newline)

(define (call-the-cops) '110)

(define (make-account-seven balance passwd)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                    balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((counter 0))
        (define (dispatch m pass)
            (if (eq? pass passwd)
                (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request: MAKE-ACCOUNT" m)))
                (begin (set! counter (+ counter 1)) 
                        (if (> counter 7)
                            (call-the-cops)
                            "Incorrect Password"))))
        dispatch))
    
(define account7 (make-account-seven 100 'right-password))

(writeln (account7 'withdraw 'wrong-pass))
(writeln (account7 'withdraw 'wrong-pass))
(writeln (account7 'withdraw 'wrong-pass))
(writeln (account7 'withdraw 'wrong-pass))
(writeln (account7 'withdraw 'wrong-pass))
(writeln (account7 'withdraw 'wrong-pass))
(writeln (account7 'withdraw 'wrong-pass))
(writeln (account7 'withdraw 'wrong-pass))

(display '--------3.6)
(newline)

(define (rand s) 
    (let ((x random-init))
        (cond ((eq? s 'generate) (begin (set! x (rand-update x)
                                        x)))
              ((eq? s 'reset) (lambda (y) (begin (set! x y)
                                                 y))))))

(display '--------3.7)
(newline)

(define (make-joint acc oldpass newpass)
    (let ((old ()))))

(define (make-account balance passwd)
    (define (withdraw amount)
        ((acc 'withdraw oldpass) amount))
    (define (deposit amount)
        ((acc 'deposit oldpass) amount))
    (define (dispatch m pass)
        (if (eq? pass newpass)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT" m)))
            "Incorrect Password"))
    dispatch)

(exit)