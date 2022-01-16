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

(exit)