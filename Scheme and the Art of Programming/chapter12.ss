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

(display '--------12.9)
(newline)

(define stack-maker
    (lambda msg
        (let ((stk '()))
            (lambda msg
                (case (1st msg)
                    ((type) "stack")
                    ((empty?) (null? stk))
                    ((push!) (for-effect-only
                                (set! stk (cons (2nd msg) stk))))
                    ((top) (if (null? stk)
                               (error "top: The stack is empty.")
                               (car stk)))
                    ((pop!) (for-effect-only
                                (if (null? stk)
                                    (error "pop!: The stack is empty.")
                                    (set! stk (cdr stk)))))
                    ((size) (length stk))
                    ((print) (display "TOP: ")
                                (for-each
                                    (lambda (x)
                                        (display x)
                                        (display " "))
                                    stk)
                                (newline))
                    (else (delegate base-object msg)))))))

(define correct-nested
    (lambda (ls)
        (let ((stk (stack-maker)))
            (letrec ((helper (lambda (ls)
                                (if (null? ls)
                                    (send stk 'empty?)
                                    (cond ((or (number? (car ls)) (symbol? (car ls))) (helper (cdr ls)))
                                          ((or (string=? (car ls) "(")
                                               (string=? (car ls) "{")
                                               (string=? (car ls) "[")) (send stk 'push! (car ls)) (helper (cdr ls)))
                                          ((or (string=? (car ls) ")")
                                               (string=? (car ls) "}")
                                               (string=? (car ls) "]")) (if (send stk 'empty?)
                                                                        #f
                                                                        (let ((s (send stk 'top))
                                                                            (x (car ls)))
                                                                        (cond ((and (string=? x ")") (not (string=? s "("))) #f)
                                                                            ((and (string=? x "]") (not (string=? s "["))) #f)
                                                                            ((and (string=? x "}") (not (string=? s "{"))) #f)
                                                                            (else (begin (send stk 'pop!)
                                                                                         (helper (cdr ls))))))))
                                            (else (helper (cdr ls))))))))
                (helper ls)))))

(define exp1 '(13 + 5 * "{" "[" 14 - 3 * "(" 12 - 7 ")" "]" - 15 "}"))
(writeln (correct-nested exp1))
(define exp2 '(13 + 5 * "[" "[" 14 - 3 * "(" 12 - 7 ")" "]" - 15 "}"))
(writeln (correct-nested exp2))

(display '--------12.10)
(newline)
(display '--------12.11)
(newline)

(define queue-maker
    (lambda ()
        (let ((q '()))
            (lambda msg
                (case (1st msg)
                    ((type) "queue")
                    ((empty?) (null? q))
                    ((enqueue!) (for-effect-only
                                    (let ((list-of-item (cons (2nd msg) '())))
                                        (if (null? q)
                                            (set! q list-of-item)
                                            (append! q list-of-item)))))
                    ((enqueue-list!) (for-effect-only
                                        (let ((list-of-item (2nd msg)))
                                            (if (null? q)
                                                (set! q list-of-item)
                                                (append! q list-of-item)))))
                    ((enqueue-many!) (for-effect-only
                                        (let ((list-of-item (cdr msg)))
                                            (if (null? q)
                                                (set! q list-of-item)
                                                (append! q list-of-item)))))
                    ((front) (if (null? q)
                                (error "front: The queue is empty.")
                                (car q)))
                    ((dequeue!) (for-effect-only
                                    (if (null? q)
                                        (error "dequeue!: The queue is empty.")
                                        (set! q (cdr q)))))
                    ((size) (length q))
                    ((print) (display "FRONT: ")
                             (for-each
                                (lambda (x) (display x) (display " "))
                                            q)
                             (newline))
                    (else (delegate base-object msg)))))))

(define q1 (queue-maker))
(send q1 'enqueue-list! '(1 2 3))
(writeln (send q1 'print))
(send q1 'enqueue-list! '(4 5 6))
(writeln (send q1 'print))

(define q2 (queue-maker))
(send q2 'enqueue-many! 1 2 3)
(writeln (send q2 'print))
(send q2 'enqueue-many! 4 5 6)
(writeln (send q2 'print))

(display '--------12.12)
(newline)

(define marker (list '()))

(define (queue->list q)
    (send q 'enqueue! marker)
    (letrec ((helper (lambda ()
                        (if (send q 'empty?)
                            '()
                            (let ((front (send q 'front)))
                                (if (equal? front marker)
                                    '()
                                    (begin (send q 'enqueue! front)
                                           (send q 'dequeue!)
                                           (cons front (helper)))))))))
        (helper)))

(define q3 (queue-maker))
(send q3 'enqueue-list! '(1 2 3 4 5 6))
(send q3 'print)
(writeln (queue->list q3))

(display '--------12.13)
(newline)

(define (queue->list-size q)
    (let ((size (send q 'size)))
        (letrec ((helper (lambda (i)
                        (if (send q 'empty?)
                            '()
                            (if (equal? i size)
                                '()
                                (let ((front (send q 'front)))
                                    (begin (send q 'enqueue! front)
                                       (send q 'dequeue!)
                                       (cons front (helper (add1 i))))))))))
            (helper 0))))

(define q4 (queue-maker))
(send q4 'enqueue-list! '(1 2 3 4 5 6))
(send q4 'print)
(writeln (queue->list-size q4))

(display '--------12.15)
(newline)

(define circular-list-maker 
    (lambda ()
        (let ((marker '())
              (size-gauge (gauge-maker add1 sub1)))
            (lambda msg
                (case (1st msg)
                      ((type) "circular list")
                      ((empty?) (null? marker))
                      ((insert!) (send size-gauge 'up!)
                                 (for-effect-only
                                    (if (null? marker)
                                        (begin
                                            (set! marker (cons (2nd msg) '()))
                                            (set-cdr! marker marker))
                                        (set-cdr! marker (cons (2nd msg) (cdr marker))))))
                      ((head) (if (null? marker)
                                (error "head: The list is empty.")
                                (car (cdr marker))))
                      ((delete!) (for-effect-only
                                    (if (null? marker)
                                        (error "delete!: The circular list is empty.")
                                        (begin
                                            (send size-gauge 'down!)
                                            (if (eq? marker (cdr marker))
                                                (set! marker '())
                                                (set-cdr! marker (cdr (cdr marker))))))))
                    ((move!) (for-effect-only
                                    (if (null? marker)
                                        (error "move!: The circular list is empty.")
                                        (set! marker (cdr marker)))))
                    ((size) (send size-gauge 'show))
                    ((print) (if (not (null? marker))
                                 (let ((next (cdr marker)))
                                    (set-cdr! marker '())
                                    (for-each (lambda (x) (display x) (display " "))
                                                next)
                                    (set-cdr! marker next)))
                                 (newline))
                    (else (delegate base-object msg)))))))

(define stack-maker 
    (lambda ()
        (let ((c (circular-list-maker)))
            (lambda msg
                (case (1st msg)
                    ((type) "stack")
                    ((push!) (send c 'insert! (2nd msg)))
                    ((pop!) (send c 'delete!))
                    ((top) (send c 'head))
                    ((print) (display "TOP: ") (send c 'print))
                    ((empty? size) (delegate c msg))
                    (else (delegate base-object msg)))))))

(define queue-maker 
    (lambda ()
        (let ((c (circular-list-maker))) 
            (lambda msg
                (case (1st msg)
                      ((type) "queue")
                      ((enqueue!) (send c 'insert! (2nd msg)) (send c 'move!))
                      ((dequeue!) (send c 'delete!))
                      ((front) (send c 'head))
                      ((print) (display "FRONT: ") (send c 'print))
                      ((empty? size) (delegate c msg))
                      (else (delegate base-object msg)))))))


(display '--------12.18)
(newline)

(define circular-list-maker-local
    (lambda ()
        (let ((marker '())
              (count 0))
            (lambda msg
                (case (1st msg)
                      ((type) "circular list")
                      ((empty?) (null? marker))
                      ((insert!) (set! count (+ count 1))
                                 (for-effect-only
                                    (if (null? marker)
                                        (begin
                                            (set! marker (cons (2nd msg) '()))
                                            (set-cdr! marker marker))
                                        (set-cdr! marker (cons (2nd msg) (cdr marker))))))
                      ((head) (if (null? marker)
                                (error "head: The list is empty.")
                                (car (cdr marker))))
                      ((delete!) (for-effect-only
                                    (if (null? marker)
                                        (error "delete!: The circular list is empty.")
                                        (begin
                                            (set! count (- count 1))
                                            (if (eq? marker (cdr marker))
                                                (set! marker '())
                                                (set-cdr! marker (cdr (cdr marker))))))))
                      ((move!) (for-effect-only
                                      (if (null? marker)
                                          (error "move!: The circular list is empty.")
                                          (set! marker (cdr marker)))))
                      ((size) count)
                      ((print) (if (not (null? marker))
                                    (let ((next (cdr marker)))
                                         (set-cdr! marker '())
                                         (for-each (lambda (x) (display x) (display " "))
                                                  next)
                                         (set-cdr! marker next)))
                                (newline))
                      (else (delegate base-object msg)))))))

(define cl (circular-list-maker-local))
(send cl 'insert! 1)
(send cl 'insert! 2)
(send cl 'insert! 3)
(writeln (send cl 'size))

(define circular-list-maker-no-var
    (lambda ()
        (let ((marker '()))
            (lambda msg
                (case (1st msg)
                      ((type) "circular list")
                      ((empty?) (null? marker))
                      ((insert!) (for-effect-only
                                    (if (null? marker)
                                        (begin
                                            (set! marker (cons (2nd msg) '()))
                                            (set-cdr! marker marker))
                                        (set-cdr! marker (cons (2nd msg) (cdr marker))))))
                      ((head) (if (null? marker)
                                (error "head: The list is empty.")
                                (car (cdr marker))))
                      ((delete!) (for-effect-only
                                    (if (null? marker)
                                        (error "delete!: The circular list is empty.")
                                        (if (eq? marker (cdr marker))
                                            (set! marker '())
                                            (set-cdr! marker (cdr (cdr marker)))))))
                      ((move!) (for-effect-only
                                      (if (null? marker)
                                          (error "move!: The circular list is empty.")
                                          (set! marker (cdr marker)))))
                      ((size) (if  (null? marker)
                                   0
                                   (let ((next (cdr marker)))
                                      (set-cdr! marker '())
                                      (let ((c (length next))) 
                                        (set-cdr! marker next)
                                        c))))
                      ((print) (if (not (null? marker))
                                   (let ((next (cdr marker)))
                                      (set-cdr! marker '())
                                      (for-each (lambda (x) (display x) (display " "))
                                                  next)
                                        (set-cdr! marker next)))
                                (newline))
                                   
                      (else (delegate base-object msg)))))))

(define cnv (circular-list-maker-no-var))
(send cnv 'insert! 1)
(send cnv 'insert! 2)
(send cnv 'insert! 3)
(writeln (send cnv 'size))
(writeln (send cnv 'print))
(exit)