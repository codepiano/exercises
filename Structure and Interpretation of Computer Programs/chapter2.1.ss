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

(display '--------2.4)
(newline)

(define (mycons x y) (lambda (m) (m x y)))

(define (mycar z) (z (lambda (p q) p)))

(define (mycdr z) (z (lambda (p q) q)))

(display '--------2.5)
(newline)

(define (ncons a b)
    (* (expt 2 a) (expt 3 b)))

(define (divide-until x y)
    (if (= (remainder x y) 0)
        (divide-until (/ x y) y)
        x))

(define (ncar x) (/ (log (divide-until x 3)) (log 2)))

(define (ncdr x) (/ (log (divide-until x 2)) (log 3)))

(writeln (ncar (ncons 7 9)))
(writeln (ncdr (ncons 7 9)))

(display '--------2.6)
(newline)

(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus a b)
    (lambda (f) (lambda (x) ((b f) ((a f) x)))))

(display '--------2.7)
(newline)

(define (make-interval a b) (cons a b))

(define (upper-bound x) (max (car x) (cdr x)))

(define (lower-bound x) (min (car x) (cdr x)))

(display '--------2.8)
(newline)

(define (sub-interval x y)
            (make-interval (abs (- (lower-bound x) (lower-bound y)))
                           (abs (- (upper-bound x) (upper-bound y)))))

(display '--------2.10)
(newline)

(define (div-interval x y) (mul-interval
    x
    (make-interval (/ 1.0 (if (= 0 (upper-bound y))
                              (error "upper bound is zero")
                              (upper-bound y)))
                   (/ 1.0 (if (= 0 (lower-bound y))
                              (error "lower bound is zero")
                              (lower-bound y))))))

(display '--------2.12)
(newline)

(define (make-center-percent c p)
    (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent x)
    (/ (- (upper-bound x) (center x)) (center x)))

(exit)