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

(display '--------17.2)
(newline)

(define receiver
    (lambda (continuation)
        (continuation continuation)))

(define tester
    (lambda (continuation)
        (writeln "beginning")
        (call/cc continuation)
        (writeln "middle")
        (call/cc continuation)
        (writeln "end")))

(tester (call/cc receiver))

#| 输出:
> beginning

先执行 (call/cc receiver)， 构造的 continuation 是 <ep>:  (escaper (lambda (x) (tester x)))，等价于 (escaper tester)
(call/cc receiver) 等价于 (receiver <ep>)，等于 (<ep> <ep>) ，等于 (tester <ep>)，第一次调用 tester，此时参数 continuation 的值是 <ep>
执行 tester 中的 (writeln "beginning") 输出 beginning，

> beginning

执行 tester[(call/cc receiver) 中调用] 中的第二行： (call/cc continuation)，此时 continuation 是 <ep>，(call/cc <ep>) 构造的 continuation 是 <epa>， 如下：
(escaper
    (lambda (x)
        x
        (writeln "middle")
        (call/cc <ep>)
        (writeln "end")))

(call/cc <ep>) 等价于 (<ep> <epa>)，等价于 ((escaper tester) <epa>)，第二次调用 tester，此时 continuation 的值是 <epa>
执行 tester 中的 (writeln "beginning") 输出 beginning，

; middle

执行 tester[<ep>中调用] 的第二行： (call/cc continuation)，此时 continuation 是 <epa>，(call/cc <epa>) 构造的 continuation 是 <epb>，如下：

(escaper
    (lambda (x)
        x
        (writeln "middle")
        (call/cc <epa>)
        (writeln "end")))

(call/cc <epa>) 等价于 (<epa> <epb>)，<epa> 没有对传入的参数 <ebp> 执行动作，然后执行 (writeln "middle") 输出 "middle"

; beginning

执行 tester(<epa>) 的第四行 (call/cc <ep>)，构造的 continuation 是 <epc>，如下：

(escaper
    (lambda (x)
        x
        (writeln "end")))

(call/cc <ep>) 等价于 (<ep> <epc>)，等价于 ((escaper tester) <epc>)，第三次调用 tester，此时 continuation 的值是 <epc>
执行 tester 中的 (writeln "beginning") 输出 "beginning"

; end

执行 tester[<ep>中调用] 的第二行，(call/cc contiuation)，此时 continuation 的值是 <epc>，(call/cc <epc>)构造的 continuation 是 <epd>，即：

(escaper
    (lambda (x)
        x
        (writeln "middle")
        (call/cc <epc>)
        (writeln "end")))

(call/cc <epc>) 等价于 (<epc> <epc>)，等价于 (escaper (writeln "end"))，输出 "end"
|#

(display "--------Escaping from and Returning to Deep Recursions")
(newline)

(define *escape/thunk* "any continuation")

(define receiver-4
    (lambda (continuation)
        (set! *escape/thunk* continuation)
        (*escape/thunk* (lambda () (writeln "escaper is defined")))))

((call/cc receiver-4))

(define escaper
    (lambda (proc)
        (lambda args
            (*escape/thunk*
                (lambda ()
                    (apply proc args))))))

(define break1
    (lambda (x)
        (let ((break-receiver
                (lambda (continuation)
                        (continuation x))))
            (call/cc break-receiver))))


(define get-back "any procedure")

(define break2
    (lambda (x)
        (let ((break-receiver (lambda (continuation)
                                (set! get-back (lambda () (continuation x)))
                                (any-action x))))
            (call/cc break-receiver))))

(define any-action
    (lambda (x)
        ((escaper (lambda () x)))
        (get-back)))

(define break
    (lambda (x)
        (let ((break-receiver (lambda (continuation)
                                (set! get-back (lambda () (continuation x)))
                                (any-action x))))
            (call/cc break-receiver))))

(define flatten-number-list
    (lambda (s)
        (cond
            ((null? s) '())
            ((number? s) (list (break s)))
            (else
                (let ((flatcar (flatten-number-list (car s))))
                    (append flatcar (flatten-number-list (cdr s))))))))

; execute in scheme interpreter

(flatten-number-list '((1 2 3) ((4 5)) (6)))

(display '--------17.6)
(newline)

(define get-back "any escape procedure")

(define break-argument "any value")

(define zero-number 0)

(define break-on-zero
    (lambda ()
        (let ((break-receiver
                (lambda (continuation)
                    (if (> zero-number 3)
                        (begin 
                            (set! zero-number 0)
                            ((escaper (lambda () (writeln "error: too many zeros")))))
                        (begin 
                            (writeln 0)
                            (set! zero-number (add1 zero-number))
                            (set! get-back (lambda (x) (continuation x)))
                            ((escaper (lambda () 1))))))))
            (call/cc break-receiver))))

(define product+
    (lambda (n ls)
        (letrec ((product
            (lambda (ls)
                (cond
                    ((null? ls) 1)
                    ((number? (car ls))
                        (* (if (zero? (car ls))
                                (break-on-zero)
                                (car ls))
                           (product (cdr ls))))
                        (else (* (product (car ls))
                                 (product (cdr ls))))))))
            (+ n (product ls)))))

(display '--------17.7)
(newline)

(define-syntax break-var (syntax-rules ()
    ((break-var var)
     (break (list (lambda () var) (lambda (v) (set! var v)))))))

(display '--------17.10)
(newline)

(define coroutine-maker
    (lambda (proc)
        (let ((saved-continuation "any continuation"))
            (let ((first-time #t))
                (lambda (value)
                    (if first-time
                        (begin
                            (set! first-time #f)
                            (proc (resume-maker (lambda (v)
                                            (set! saved-continuation v))) value))
                        (saved-continuation value) ))))))

(display '--------17.11)
(newline)

(define first-time #t)

(define coroutine-maker
    (lambda (proc)
        (let ((saved-continuation "any continuation"))
            (lambda (value)
                (if first-time
                    (begin
                        (set! first-time #f)
                        (proc (resume-maker (lambda (v)
                                        (set! saved-continuation v))) value))
                    (saved-continuation value) )))))

(display '--------17.13)
(newline)

(define-syntax wrap (syntax-rules ()
    ((wrap proc)
     (lambda args (apply proc args)))))

(define args 5)

(writeln ((wrap (lambda (x y) (+ x y))) 4 6))
(writeln ((wrap (lambda (x y) (let ((args (+ x 1)))
                                    (+ args y)))) 4 6))
(writeln ((wrap (lambda (x y) (+ x y args))) 4 6))

(display '--------17.14)
(newline)

(define-syntax wrapOne (syntax-rules ()
    ((wrapOne proc)
     (lambda (arg) (proc arg)))))

(writeln ((wrapOne (lambda (x) (+ x 6))) 4))

(display '--------17.15)
(newline)

(define resume-maker
    (lambda (update-proc!)
        (lambda (next-coroutine value)
            (let ((receiver (lambda (continuation)
                                (update-proc! continuation)
                                (next-coroutine value))))
                (call/cc receiver)))))

(define reader
    (lambda (right)
        (let ((co-proc (lambda (resume v)
            (cycle-proc
                (lambda ()
                    (resume right (prompt-read "in> ")))))))
            (coroutine-maker co-proc))))

(define writer
    (lambda (left escape-on-end)
        (let ((co-proc (lambda (resume v)
                        (cycle-proc
                            (lambda ()
                                (let ((symbol (resume left 'ok)))
                                    (if (eq? symbol 'end)
                                        (escape-on-end 'end)
                                        (writeln "out> " symbol))))))))
            (coroutine-maker co-proc))))

(define x->y
    (lambda (x y left right)
        (let ((co-proc (lambda (resume v)
            (cycle-proc
                (lambda ()
                    (let ((symbol-1 (resume left 'ok)))
                        (if (eq? symbol-1 x)
                            (let ((symbol-2 (resume left 'more)))
                                (if (eq? symbol-2 x)
                                    (resume right y)
                                    (begin
                                        (resume right symbol-1)
                                        (resume right symbol-2))))
                            (resume right symbol-1))))))))
            (coroutine-maker co-proc))))

(define grune
    (lambda ()
        (let ((grune-receiver
            (lambda (escape-grune)
                (letrec
                    ((Input (reader (wrapOne A)))
                     (A (x->y 'a 'b (wrapOne Input) (wrapOne B)))
                     (B (x->y 'b 'c (wrapOne A) (wrapOne Output)))
                     (Output (writer (wrapOne B) escape-grune)))
                     (Output 'ok)))))
            (call/cc grune-receiver))))

(exit)
