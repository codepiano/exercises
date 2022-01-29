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


(display '--------17.1)
(newline)

(define thaw
    (lambda (thunk)
        (thunk)))

(define (attempt th)
    (let ((receiver (lambda (proc) (list th proc))))
        (call/cc receiver)))

(define cycle-proc
    (lambda (th)
        (let ((pair (attempt th)))
            (let ((v (1st pair))
                  (returner (2nd pair)))
                  (begin (thaw th)
                         (returner (list th returner)))))))

; forever
; (cycle-proc (lambda () (writeln 1)))