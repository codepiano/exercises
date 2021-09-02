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

(display '--------9.7)
(newline)

(define num-cols
    (lambda (mat)
        (let ((size (sub1 (vector-length mat))))
            (vector-ref mat size))))

(define num-rows
    (lambda (mat)
        (let ((size (sub1 (vector-length mat))))
            (/ size (vector-ref mat size)))))

(define matrix-ref
    (lambda (mat)
        (let ((ncols (num-cols mat)))
            (lambda (i j)
                (if (or (> j ncols) (> i (num-rows mat)))
                    (writeln "matrix-ref: The ref is illegal.")
                    (vector-ref mat (+ (* i ncols) j)))))))

(writeln ((matrix-ref '#(1 2 3 4 5 6 7 8 2)) 3 0))
(writeln ((matrix-ref '#(1 2 3 4 5 6 7 8 3)) 3 0))
(writeln ((matrix-ref '#(1 2 3 4 5 6 7 8 3)) 2 8))

(display '--------9.8)
(newline)

(define matrix-generator
    (lambda (gen-proc)
        (lambda (nrows ncols)
            (let ((size (* nrows ncols)))
                (let ((vec-gen-proc
                    (lambda (k)
                        (if (< k size)
                            (gen-proc (quotient k ncols)
                                      (remainder k ncols))
                            ncols))))
                ((vector-generator vec-gen-proc)
                    (add1 size)))))))

(define (matrix m n)
    (lambda args
        ((matrix-generator (lambda (i j)
                                (list-ref args (+ (* n i) j)))) m n)))

(writeln ((matrix 3 4) 5 2 3 7 1 4 0 5 8 3 1 2))

(display '--------9.9)
(newline)

(define vector-sum
    (lambda (vec)
        (let ((size (vector-length vec)))
            (letrec
                ((helper (lambda (i)
                            (if (= i size)
                                0
                                (+ (vector-ref vec i) (helper (addl i)))))))
                (helper 0)))))

(define matrix-apply-elementwise-to-both
    (lambda (proc)
        (lambda (vec1 vec2)
            (let* ((hi (sub1 (vector-length vec1)))
                   (gen-proc
                        (lambda (i)
                            (if (= i hi)
                                (vector-ref vec1 i)
                                (proc (vector-ref vec1 i) (vector-ref vec2 i))))))
                ((vector-generator gen-proc) (vector-length vec1))))))

(define mat+ (matrix-apply-elementwise-to-both +))

(writeln (mat+ '#(1 2 3 4 5 6 7 8 9 3) '#(9 8 7 6 5 4 3 2 1)))

(display '--------9.10)
(newline)

(define matrix-map
    (lambda (proc vec)
        (let ((hi (sub1 (vector-length vec))))
            ((vector-generator (lambda (i) 
                                (if (= i hi)
                                    (vector-ref vec i)
                                    (proc (vector-ref vec i)))))
            (vector-length vec)))))

(define matrix-multiply-by-scalar (lambda (c vec)
    (matrix-map (lambda (elem) (* c elem)) vec)))

(writeln (matrix-multiply-by-scalar 10 '#(1 2 3 4 5 6 7 8 9 3)))


(display '--------9.11)
(newline)

(define matrix-view
    (lambda (vec)
        (let ((hi (sub1 (sub1 (vector-length vec))))
              (col (num-cols vec)))
            (letrec ((loop (lambda (i)
                (if (> (vector-length vec) 0)
                    (display (vector-ref vec i)))
                (if (= 0 (remainder (add1 i) col))
                    (newline))
                (if (< i hi)
                    (begin
                        (if (not (= (remainder (add1 i) col) 0))
                            (display " "))
                        (loop (add1 i) ))))))
        (loop 0)))))

(matrix-view '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 4))

(display '--------9.13)
(newline)

(define num-rows-v vector-length)

(define (num-cols-v v)
    (vector-length (vector-ref v 0)))

(define matrix-ref-v
    (lambda (mat)
        (let ((ncols (num-cols-v mat)))
            (lambda (i j)
                (if (or (> j ncols) (> i (num-rows-v mat)))
                    (writeln "matrix-ref: The ref is illegal.")
                    (vector-ref (vector-ref mat i) j))))))

(define matrix-set!-v
    (lambda (mat)
        (let ((ncols (num-cols-v mat)))
            (lambda (i j val)
                (if (or (> j ncols) (> i (num-rows-v mat)))
                    (writeln "matrix-ref: The ref is illegal.")
                    (vector-set! (vector-ref mat i) j val))))))

(define matrix-generator-v
    (lambda (gen-proc)
        (lambda (nrows ncols)
            (let ((vec (make-vector nrows)))
                (letrec ((row (sub1 nrows))
                        (loop (lambda (n)
                                    (if (> n row)
                                        vec
                                        (begin (vector-set! vec n
                                                    ((vector-generator (lambda (x) (gen-proc n x))) ncols))
                                               (loop (add1 n)))))))
                    (loop 0))))))


(define (matrix-v m n)
    (lambda args
        ((matrix-generator-v (lambda (i j)
                                (list-ref args (+ (* n i) j)))) m n)))

(writeln ((matrix-v 3 4) 5 2 3 7 1 4 0 5 8 3 1 2))

(exit)