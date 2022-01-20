(define writeln (lambda x (for-each display x) (newline)))

(define 1st car)
(define 2nd cadr)

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(display '--------14.1)
(newline)

(define make-lambda-expression
    (lambda (parameters body-expressions)
        (cons 'lambda (cons parameters body-expressions))))

(define freeze-transformer (lambda (code)
    (make-lambda-expression '() (list (2nd code)))))

(let-syntax ((freeze (syntax-rules ()
                     ((freeze expr1 expr2 ...)
                      (lambda ()
                          expr1 expr2 ...)))))
    (writeln (freeze-transformer '(freeze (cons 'a '(b c))))))             

(define make-list-of-parameters
  (lambda (code)
    (map 1st (2nd code))))

(define make-list-of-operands
  (lambda (code)
    (map 2nd (2nd code))))

(define make-list-of-body-items
  (lambda (code)
    (cddr code)))

(define let-transformer
  (lambda (code)
    (cons (make-lambda-expression (make-list-of-parameters code) (make-list-of-body-items code)) (make-list-of-operands code))))

(let-syntax ((mlet (syntax-rules ()
                     ((mlet ((var val) ...) expr1 expr2 ...)
                      ((lambda (var ...)
                          expr1 expr2 ...) val ...)))))
    (writeln (let-transformer '(mlet ((a 5) (b 2)) (* a b)))))

(display '--------14.2)
(newline)

; let
(let-syntax ((mletrec (syntax-rules ()
                        ((mletrec ((var val) ...) expr1 expr2 ...)
                         (let ((var "any") ...)
                            (set! var val) ...
                            expr1 expr2 ...)))))
    (writeln (mletrec ((a 1)
                       (b (+ a 1)) )
                    (+ a b))))


; no let
(let-syntax ((mletrec (syntax-rules ()
                        ((mletrec ((var val) ...) expr1 expr2 ...)
                         ((lambda (var ...)
                            (set! var val) ...
                            expr1 expr2 ...) (lambda () val) ...)))))
    (writeln (mletrec ((a 1)
                       (b (+ a 1)))
                    (+ a b))))

(display '--------14.4)
(newline)

(define and-proc 
    (lambda (th-list)
        (cond ((null? th-list) #f)
              ((null? (cdr th-list)) ((car th-list)))
              (else (let ((v ((car th-list))))
                        (if v
                            (and-proc (cdr th-list))
                            #f))))))

(let-syntax ((mand (syntax-rules ()
                        ((mand e ...)
                         (and-proc (list (lambda () e) ...))))))
    (writeln (mand))
    (writeln (mand #t))
    (writeln (mand #f))
    (writeln (mand #t #t #t))
    (writeln (mand #t #t #f)))

(display '--------14.5)
(newline)

(let-syntax ((mlet* (syntax-rules ()
                        ((mlet* ((var1 val1)) expr1 expr2 ...)
                         (let ((var1 val1))
                            expr1 expr2 ...))
                        ((mlet* ((var1 val1) (var2 val2) ...) expr1 expr2 ...)
                         (let ((var1 val1))
                            (mlet* ((var2 val2) ...) expr1 expr2 ...))))))
    (writeln (mlet* ((a 1)
                     (b (+ a 2))
                     (c (* a b)))
                     (+ a (- c b)))))

(display '--------14.8)
(newline)

(let-syntax ((mlet (syntax-rules ()
                     ((mlet ((var val) ...) expr1 expr2 ...)
                      ((lambda (var ...)
                          expr1 expr2 ...) val ...))
                     ((mlet name ((var val) ...) expr1 expr2 ...)
                      ((letrec ((name (lambda (var ...)
                                        expr1 expr2 ...))) name) val ...)))))
    (writeln (mlet test ((a 1) (b 2)) (+ a b))))


(display '--------14.9)
(newline)

(define thaw
    (lambda (thunk)
        (thunk)))

(let-syntax ((cycle (syntax-rules ()
                        ((cycle expr1 expr2 ...)
                         ((lambda (th)
                            (letrec ((loop (lambda (n)
                                                (thaw th)
                                                (if (> n 10)
                                                    (writeln "force break cycle")
                                                    (loop (+ n 1))))))
                                (loop 0))) (lambda () expr1 expr2 ...))))))
    (writeln (cycle (writeln 1))))

(display '--------14.10)
(newline)

(let-syntax ((while (syntax-rules ()
                     ((while test expr1 expr2 ...)
                      (letrec ((loop (lambda ()
                                        (if test
                                            (begin expr1 expr2 ... (loop))))))
                            (loop))))))
    (writeln (let ((loop 100) (sum 0))
                (while (positive? loop)
                       (set! sum (+ sum loop))
                       (set! loop (sub1 loop)))
                sum)))

(display '--------14.11)
(newline)

(let-syntax ((repeat (syntax-rules ()
                     ((repeat expr test)
                      (letrec ((loop (lambda ()
                                        (begin expr
                                               (if test
                                                   (loop))))))
                            (loop))))))
    (writeln (let ((loop 0))
                (repeat (set! loop (sub1 loop))
                        (positive? loop))
                loop)))

(define-syntax while (syntax-rules ()
                     ((while test expr1 expr2 ...)
                      (letrec ((loop (lambda ()
                                        (if test
                                            (begin expr1 expr2 ... (loop))))))
                            (loop)))))

(letrec-syntax ((repeat (syntax-rules ()
                            ((repeat expr test)
                             (begin expr
                                    (while test expr))))))
    (writeln (let ((loop 0))
                (repeat (set! loop (sub1 loop))
                        (positive? loop))
                loop)))

(display '--------14.12)
(newline)

(define-syntax for (syntax-rules ()
                            ((for var initial step test expr1 expr2 ...)
                             (let ((var initial))
                                (let ((step-thunk (lambda () step))
                                      (test-thunk (lambda () test))
                                      (body-thunk (lambda () expr1 expr2 ...)))
                                    (while (not (thaw test-thunk))
                                           (thaw body-thunk)
                                           (set! var (thaw step-thunk))))))))
                                        
(define vector-sum 
    (lambda (v)
        (let ((n (vector-length v))
              (sum 0))
                (for i 0 (add1 i) (= i n) (set! sum (+ sum (vector-ref v i))))
                sum)))

(writeln (vector-sum '#(1 2 3 4)))

(display '--------14.13)
(newline)

(define-syntax do (syntax-rules ()
                    ((do ((var initial step) ...)
                        (test exit1 exit2 ...)
                        (expr1 expr2 ...))
                     ((letrec ((loop (lambda (var ...)
                                        (cond (test exit1 exit2 ...)
                                              (else (begin expr1 expr2 ...)
                                                    (loop step ...))))))
                            loop) initial ...))))

(define-syntax fordo (syntax-rules ()
                        ((fordo var initial step test expr1 expr2 ...)
                         (do ((var initial step))
                            (test #t)
                            (expr1 expr2 ...)))))

(define vector-sum-fordo
    (lambda (v)
        (let ((n (vector-length v))
              (sum 0))
                (fordo i 0 (add1 i) (= i n) (set! sum (+ sum (vector-ref v i))))
                sum)))

(writeln (vector-sum-fordo '#(1 2 3 4)))

(display '--------14.14)
(newline)

(define (begin0-proc first others)
    (let ((result first)
          (hole (others)))
        result))

(let-syntax ((begin0 (syntax-rules ()
                     ((begin0 expr)
                      expr)
                     ((begin0 expr1 expr2 expr3 ...)
                      (begin0-proc expr1 (lambda () expr2 expr3 ...))))))
    (writeln (begin0 (writeln 1) (writeln 2) (writeln 3)))
    (writeln (begin0 (writeln 1))))

(display '--------14.16)
(newline)

(define member-trace
    (lambda (item ls)
        (if (null? ls)
            (begin (writeln "no") #f)
            (if (equal? (car ls) item)
                (begin (writeln "yes") #t)
                (begin (writeln "maybe")
                       (member-trace item (cdr ls)))))))

(define factorial
    (lambda (n)
        (if (zero? n)
            (begin 1)
            (begin (* n (factorial (sub1 n)))))))

(display '--------14.17)
(newline)

(define-syntax mcond (syntax-rules ()
                        ((mcond (else e1 e2 ...))
                         (begin e1 e2 ...))
                        ((mcond (test e1 e2 ...) clauses ...)
                         (if test
                             (begin e1 e2 ...)
                             (cond clauses ...)))))

(define x 1)
(writeln (mcond ((> x 10) x)
                ((= x 10) 100)
                (else 0)))

(display '--------14.18)
(newline)

(define-syntax variable-case (syntax-rules ()
     ((variable-case var (else e1 e2 ...)) 
      (begin e1 e2 ...))
     ((variable-case var (keys e1 e2 ...) clauses ...)
      (if (memv var (quote keys))
        (begin e1 e2 ...)
        (variable-case var clauses ...)))))

(let ((x (remainder 35 10)))
    (variable-case x
        ((2 4 6 8) (writeln "even") x)
        ((13 5 7 9) (writeln "odd") x)
        (else (writeln "zero") x)))

(display '--------14.19)
(newline)

(define target #t)

(define-syntax mcase (syntax-rules ()
     ((mcase (else e1 e2 ...)) 
      (begin e1 e2 ...))
     
     ((mcase e1
        (keys e2 e3 ...) clauses ...)
      (begin (set! target ((lambda () e1)))
             (writeln 1)
             (writeln target)
             (writeln (quote keys))
             (if (memv target (quote keys))
                     (begin e2 e3 ...)
                     (variable-case clauses ...))))
     ((mcase (keys e1 e2 ...) clauses ...)
     (begin (writeln 2)
     (if (memv target (quote keys))
              (begin e1 e2 ...)
              (variable-case clauses ...)))) ))

(mcase (remainder 35 10)
    ((2 4 6 8) (writeln "even") target)
    ((13 5 7 9) (writeln "odd") target)
    (else (writeln "zero") target))

(display '--------14.20)
(newline)

(define-syntax object-maker (syntax-rules ()
    ((object-maker (e3 e4 ...) (keys e1 e2 ...) clauses ...)
     (lambda msg (case (1st msg)
                    ((keys e1 e2 ...) clauses ...)
                    (else (e3 e4 ...)))))))

(exit)