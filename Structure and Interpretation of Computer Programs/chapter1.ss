(define writeln (lambda x (for-each display x) (newline)))

(display '--------1.2)
(newline)

(writeln (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))

(display '--------1.3)
(newline)

(define square-sum-of-max-two
    (lambda (a b c)
        (- (+ (square a) (square b) (square c)) (square (min a b c)))))

(writeln (square-sum-of-max-two 1 2 3))

(display '--------1.5)
(newline)

; recursive forever
(define (p) (p))

(define (test x y)
    (if (= x 0) 0 y))

; calc arguments (p), will not stop
; (writeln (test 0 (p))) 

(display '--------1.6)
(newline)

(writeln "applicative-order")

(display '--------1.7)
(newline)

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x) (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (mysqrt x) (sqrt-iter 1.0 x))

(writeln (mysqrt 0.002))
(writeln (sqrt 0.002))

(writeln (mysqrt 10001))
(writeln (sqrt 10001))

(define (new-good-enough? guess x)
    (< (/ (abs (- guess (improve guess x))) guess) 0.00001))

(define (new-sqrt-iter guess x)
    (if (new-good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x) x)))

(define (new-mysqrt x) (new-sqrt-iter 1.0 x))

(writeln "new good enough:")

(writeln (new-mysqrt 0.002))
(writeln (sqrt 0.002))

(writeln (new-mysqrt 10001))
(writeln (sqrt 10001))

(display '--------1.8)
(newline)

(define (cube-improve y x) (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cube-good-enough? guess x)
    (< (abs (- (* guess guess guess) x)) 0.001))

(define (cube-iter guess x)
    (if (cube-good-enough? guess x)
      guess
      (cube-iter (cube-improve guess x) x)))

(writeln (cube-iter 1.0 5))

(exit)