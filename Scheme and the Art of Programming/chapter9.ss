(define writeln (lambda x (for-each display x) (newline)))

(define compose-unrestrict 
    (lambda (f g)
        (lambda args
            (f (apply g args)))))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(display '--------9.1)
(newline)

(define vector-generator
    (lambda (gen-proc)
        (lambda (size)
            (let ((vec (make-vector size)))
                (letrec
                    ((loop (lambda (i)
                        (if (< i size)
                            (begin
                                (vector-set! vec i (gen-proc i))
                                (loop (add1 i)))))))
                    (loop 0))
                vec))))

(define (successive-powers base)
    (lambda (n)
       ((vector-generator (lambda (x) (expt base x))) n)))

(writeln ((successive-powers 2) 8))
(writeln ((successive-powers 3) 5))

(display '--------9.2)
(newline)

(define view
    (lambda (vec)
        (let ((highest-index (sub1 (vector-length vec))))
            (letrec ((loop (lambda (i)
                (if (> (vector-length vec) 0)
                    (display (vector-ref vec i)))
                (if (< i highest-index)
                    (begin
                        (display " ")
                        (loop (add1 i) ))))))
        (display "#(")
        (loop 0)
        (display ")")))))

(view '#())
(newline)
(view '#(1 2))
(newline)

(display '--------9.3)
(newline)

(define vector-view
    (lambda (vec)
        (let ((highest-index (sub1 (vector-length vec))))
            (letrec ((loop (lambda (i)
                (if (> (vector-length vec) 0)
                    (display (vector-ref vec i)))
                (if (< i highest-index)
                    (begin
                        (display ", ")
                        (loop (add1 i) ))))))
        (display "#<")
        (loop 0)
        (display ">")))))

(vector-view '#())
(newline)
(vector-view '#(1 2))
(newline)

(display '--------9.5)
(newline)

(define (vector-linear-search vec obj)
    (let ((highest-index (sub1 (vector-length vec))))
        (letrec ((loop (lambda (i)
                        (cond ((= highest-index -1) -1)
                              ((> i highest-index) -1)
                              (else (if (equal? (vector-ref vec i) obj)
                                        i
                                        (loop (add1 i))))))))
                (loop 0))))

(writeln (vector-linear-search '#(g n p r a d l b s) 'a))
(writeln (vector-linear-search '#(29 13 96 -5 24 11 9 -15 2) 11))

(display '--------9.6)
(newline)

(define vector-append
    (lambda (vec vec2)
        (let ((size (vector-length vec))
              (new-size (+ (vector-length vec) (vector-length vec2))))
            (let* ((gen-proc (lambda (i)
                                (if (< i new-size)
                                    (if (< i size)
                                        (vector-ref vec i)
                                        (vector-ref vec2 (- i size)))
                                    '()))))
                ((vector-generator gen-proc) new-size)))))

(writeln (vector-append '#(1 2 3 4 5 6) '#(7 8 9 10)))

(define vector-reverse
    (lambda (vec)
        (let ((hi (sub1 (vector-length vec))))
            (let* ((gen-proc (lambda (i)
                                    (if (<= i hi)
                                        (vector-ref vec (- hi i))
                                        '()))))
                ((vector-generator gen-proc) (+ hi 1))))))

(writeln (vector-reverse '#(1 2 3 4 5 6)))

(display '--------9.6)
(newline)

(exit)