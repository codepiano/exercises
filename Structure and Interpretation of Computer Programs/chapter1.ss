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

(display '--------1.11)
(newline)

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(writeln (f 10))

(define (f-iter n a b c)
    (if (= 0 n)
        a
        (f-iter (- n 1) (+ a (* 2 b) (* 3 c)) a b)))

(writeln (f-iter 8 2 1 0))

(display '--------1.12)
(newline)

(define (yang a)
    (cond ((null? a) '())
          ((null? (cdr a)) '(1))
          (else (cons (+ (car a) (cadr a)) (yang (cdr a))))))

(define (pascal n a)
    (if (= n 0)
        a
        (cons a (pascal (- n 1) (cons 1 (yang a))))))

(writeln (pascal 4 '(1)))

(display '--------1.16)
(newline)

(define (exp-iter b a n)
    (cond ((= n 0) a)
          ((even? n) (exp-iter (square b) a (/ n 2)))
          ((odd? n) (exp-iter b (* a b) (- n 1)))))

(writeln (exp-iter 2 1 10))

(display '--------1.17)
(newline)

(define (halve n) (/ n 2))
(define (double n) (+ n n))

(define (my* a b)
    (cond ((= b 0) 0)
          ((= b 1) a)
          ((even? b) (my* (double a) (halve b)))
          ((odd? b) (+ a (my* a (- b 1))))))

(writeln (my* 3 7))

(display '--------1.18)
(newline)

(define (*-iter a b c)
    (cond ((= b 0) 0)
          ((= b 1) (+ a c))
          ((even? b) (*-iter (double a) (halve b) c))
          ((odd? b) (*-iter a (- b 1) (+ c a)))))

(writeln (*-iter 3 7 0))


(display '--------1.19)
(newline)

(define (fibr n)
    (fibr-iter 1 0 n))

(define (fibr-iter a b count)
    (if (= count 0)
        b
        (fibr-iter (+ a b) a (- count 1))))


(define (fib n) (fib-iter 1 0 0 1 n))

; a = [(a+b)q + ap]
; b = (bp + aq)
; a' = {[(a+b)q + ap] + (bp + aq)}q + [(a+b)q + ap]p
;       ----- a -----   --- b ---      ---- a ---- 
; a' = (a+b)(q*q+2pq) + a(p*p+q*q)
; 同样方式，可得 b' = b(p*p+q*q) + a(q*q+2pq)
; 所以 p' = (p*p+q*q) q' = (q*q+2pq)

(define (fib-iter a b p q count)
    (cond ((= count 0) b)
           ((even? count) (fib-iter a
                                    b
                                    (+ (* p p) (* q q))
                                    (+ (* q q) (* 2 p q))
                                    (/ count 2)))
           (else (fib-iter (+ (* b q) (* a q) (* a p))
                           (+ (* b p) (* a q))
                           p
                           q
                           (- count 1)))))

(writeln (fibr 7))
(writeln (fib 7))

(exit)
