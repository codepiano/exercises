(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

(display '--------2.1)
(newline)

(define (gcd a b) (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
    (define (help x y)
        (let ((g (gcd x y)))
            (cons (/ x g) (/ y g))))
    (cond ((< d 0) (help (- 0 n) (- 0 d)))
          (else (help n d))))

(writeln (make-rat 1 1))
(writeln (make-rat -1 1))
(writeln (make-rat 1 -1))
(writeln (make-rat -1 -1))

(display '--------2.2)
(newline)

(define (make-point x y)
    (cons x y))

(define x-point car)

(define y-point cdr)

(define (make-segment x y)
    (cons x y))

(define start-segment car)

(define end-segment cdr)

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(define (midpoint-segment line)
    (make-point (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)
                (/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)))

(writeln (midpoint-segment (make-segment (make-point 2 5) (make-point 7 9))))

(display '--------2.3)
(newline)

(define (make-rectangles a b c d)
    (cons (make-point a b) (make-point c d)))

(define rec-point-x car)

(define rec-point-y cdr)

(define (length a)
    (abs (- (x-point (rec-point-x a)) (x-point (rec-point-y a)))))

(define (height a)
    (abs (- (y-point (rec-point-x a)) (y-point (rec-point-y a)))))

(define (area x)
    (* (length x) (height x)))

(define (perimeter x)
    (* (+ (length x) (height x)) 2))

(define r1 (make-rectangles 0 0 2 2))

(writeln (perimeter r1))

(writeln (area r1))

(exit)