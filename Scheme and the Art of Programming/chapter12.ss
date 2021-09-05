(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define 1st car)
(define 2nd cadr)

(display '--------12.1)
(newline)

(define invalid-method-name-indicator "unknown")

(define base-object
    (lambda msg
        (case (1st msg)
            ((type) "base-object")
            (else invalid-method-name-indicator))))

(define for-effect-only
    (lambda args
        "unspecified value"))

(define delegate
    (lambda (obj msg)
        (apply obj msg)))

(define send
    (lambda args
        (let ((object (car args)) (message (cdr args)))
            (let ((try (apply object message)))
                (if (eq? invalid-method-name-indicator try)
                    (error "Bad method name:" (car message) "sent to object of" (object 'type) "type.")
                try)))))

(define box-maker
    (lambda (init-value)
        (let ((contents init-value))
            (lambda msg
                (case (1st msg)
                    ((type) "box")
                    ((show) contents)
                    ((update!) (for-effect-only (set! contents (2nd msg))))
                    ((swap!) (let ((ans contents))
                                (set! contents (2nd msg))
                                ans))
                    ((reset!) (for-effect-only (set! contents init-value)))
                    (else (delegate base-object msg)))))))

(define acc-max
    (lambda ()
        (let ((max (box-maker 0)))
            (lambda msg
                (case (1st msg)
                    ((type) "acc-max")
                    ((update!) (if (> (2nd msg) (send max 'show))
                                    (send max 'update! (2nd msg))))
                    ((show reset!) (delegate max msg))
                    (else (delegate base-object msg)))))))

(define acc-max-obj (acc-max))
(send acc-max-obj 'update! 3)
(writeln (send acc-max-obj 'show))
(send acc-max-obj 'update! 7)
(writeln (send acc-max-obj 'show))
(send acc-max-obj 'update! 2)
(writeln (send acc-max-obj 'show))
(send acc-max-obj 'update! 4)
(writeln (send acc-max-obj 'show))
(send acc-max-obj 'update! 10)
(writeln (send acc-max-obj 'show))
(send acc-max-obj 'update! 1)
(writeln (send acc-max-obj 'show))
(send acc-max-obj 'update! 5)
(writeln (send acc-max-obj 'show))

(display '--------12.2)
(newline)

(define double-box-maker
    (lambda (left right)
        (let ((lcontents left)
              (rcontents right))
            (lambda msg
                (case (1st msg)
                    ((type) "double-box")
                    ((show-left) lcontents)
                    ((show-right) rcontents)
                    ((update-left!) (for-effect-only (set! lcontents (2nd msg))))
                    ((update-right!) (for-effect-only (set! rcontents (2nd msg))))
                    ((reset!) (for-effect-only (set! lcontents left) (set! rcontents right)))
                    (else (delegate base-object msg)))))))

(define double-box-obj (double-box-maker 1 10))

(writeln (send double-box-obj 'show-left))
(writeln (send double-box-obj 'show-right))
(send double-box-obj 'update-left! 2)
(send double-box-obj 'update-right! 11)
(writeln (send double-box-obj 'show-left))
(writeln (send double-box-obj 'show-right))
(send double-box-obj 'reset!)
(writeln (send double-box-obj 'show-left))
(writeln (send double-box-obj 'show-right))

(display '--------12.3)
(newline)

(define accumulator-maker
    (lambda (init-value binary-proc)
        (let ((total (box-maker init-value)))
            (lambda msg
                (case (1st msg)
                    ((type) "accumulator")
                    ((update!) (send total 'update!
                        (binary-proc (send total 'show) (2nd msg))))
                    ((show reset!) (delegate total msg))
                    (else (delegate base-object msg)))))))

(define gauge-maker
    (lambda (init-value unary-proc-up unary-proc-down)
        (let ((total (box-maker init-value)))
            (lambda msg
                (case (1st msg)
                    ((type) "gauge")
                    ((up!) (send total 'update!
                                (unary-proc-up (send total 'show))))
                    ((down!) (send total 'update!
                                (unary-proc-down (send total 'show))))
                    ((show reset!) (delegate total msg))
                    (else (delegate base-object msg)))))))

(display '--------12.4)
(newline)

(define restricted-counter-maker
    (lambda (init-value unary-proc pred)
        (let ((total (box-maker init-value)))
            (lambda msg
                (case (1st msg)
                    ((type) "restricted-counter")
                    ((update!) (if (pred (unary-proc (send total 'show)))
                                   (send total 'update! (unary-proc (send total 'show)))
                                   (send total 'reset!)))
                    ((show reset!) (delegate total msg))
                    (else (delegate base-object msg)))))))

(define restricted-counter-obj (restricted-counter-maker 90 (lambda (x) (+ x 5)) (lambda (n) (and (> n 0) (< n 100)))))
(send restricted-counter-obj 'update!)
(writeln (send restricted-counter-obj 'show))
(send restricted-counter-obj 'update!)
(writeln (send restricted-counter-obj 'show))
(send restricted-counter-obj 'update!)
(writeln (send restricted-counter-obj 'show))

(display '--------12.5)
(newline)

(define hour-hand
    (lambda ()
        (restricted-counter-maker 0 add1 (lambda (n) (and (> n 0) (< n 13))))) )

(define hour-hand-obj (hour-hand))
(send hour-hand-obj 'update!)
(writeln (send hour-hand-obj 'show))

(display '--------12.6)
(newline)

(define modified-restricted-counter-maker
    (lambda (init-value unary-proc pred reset)
        (let ((total (box-maker init-value)))
            (lambda msg
                (case (1st msg)
                    ((type) "restricted-counter")
                    ((update!) (if (pred (unary-proc (send total 'show)))
                                   (send total 'update! (unary-proc (send total 'show)))
                                   (send total 'reset!)))
                    ((reset!) (reset total))
                    ((show) (delegate total msg))
                    (else (delegate base-object msg)))))))

(define clock-maker
    (lambda (v)
        (let* ((hour (hour-hand))
              (minite (modified-restricted-counter-maker
                        v
                        add1
                        (lambda (n) (and (> n 0) (< n 60)))
                        (lambda (x)
                            (send hour 'update!)
                            (send x 'update! 0)))))
            (lambda msg
                (case (1st msg)
                    ((type) "clock")
                    ((update!) (send minite 'update!))
                    ((show) (cons (send hour 'show) (send minite 'show)))
                    (else (delegate base-object msg)))))))

(define clock-obj (clock-maker 0))
(send clock-obj 'update!)
(writeln (send clock-obj 'show))

(display '--------12.8)
(newline)

(define counter-maker
    (lambda (init-value unary-proc)
        (let ((total (box-maker init-value)))
            (lambda msg
                (case (1st msg)
                    ((type) "counter")
                    ((update!) (let ((result (unary-proc (send total 'show))))
                                    (send total 'update! result)))
                    ((swap!) (delegate base-object msg))
                    (else (delegate total msg)))))))

(define accumulator-maker-counter
    (lambda (init-value binary-proc)
        (let* ((z 0)
              (total (counter-maker init-value (lambda (x) z))))
            (lambda msg
                (case (1st msg)
                    ((type) "accumulator")
                    ((update!) (begin (set! z (binary-proc (send total 'show) (2nd msg)))
                                      (send total 'update!)))
                    ((show reset!) (delegate total msg))
                    (else (delegate base-object msg)))))))

(define acc (accumulator-maker-counter 100 -))
(send acc 'update! 10)
(writeln (send acc 'show))
(send acc 'update! 25)
(writeln (send acc 'show))

(define gauge-maker-counter
    (lambda (init-value unary-proc-up unary-proc-down)
        (let* ((z 't)
               (total (counter-maker init-value (lambda (x) (z x)))))
            (lambda msg
                (case (1st msg)
                    ((type) "gauge") 
                    ((up!) (begin (set! z unary-proc-up)
                                  (send total 'update!)))
                    ((down!) (begin (set! z unary-proc-down)
                                  (send total 'update!)))
                    ((swap! update!) (delegate base-object msg))
                    (else (delegate total msg)))))))

(define g (gauge-maker-counter 10 add1 sub1))
(send g 'down!)
(writeln (send g 'show))
(send g 'up!)
(writeln (send g 'show))

(exit)