(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(display '--------3.50)
(newline)

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))
        
(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (force delayed-object) (delayed-object))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map (cons proc (map stream-cdr argstreams))))))

(display '--------3.54)
(newline)

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
    (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(display '--------3.55)
(newline)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums s)
    (add-streams s (cons-stream 0 (partial-sums s))))


(display '--------3.56)
(newline)

(define (scale-stream stream factor)
     (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
    (cond ((stream-null? s1) s2)
           ((stream-null? s2) s1)
           (else
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
                (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                      ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
                      (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream 2) (scale-stream 3)) (scale-stream 5)))) 

(display '--------3.64)
(newline)

(define (stream-limit stream tolerance)
        (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
               (stream-ref stream 1)
               (stream-limit (stream-cdr stream) tolerance)))

(exit)