(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

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

(display '--------1.21)
(newline)

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor) 
    (cond ((> (square test-divisor) n) n)
           ((divides? test-divisor n) test-divisor)
           (else (find-divisor n (+ test-divisor 1)))))
           
(define (divides? a b) (= (remainder b a) 0))

(writeln (smallest-divisor 199))
(writeln (smallest-divisor 1999))
(writeln (smallest-divisor 19999))

(display '--------1.22)
(newline)

(define (prime? n)
    (= n (smallest-divisor n)))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
        
(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (search-for-primes n count)
    (cond ((= count 0) n)
          ((prime? n) (begin
                        (timed-prime-test n)
                        (search-for-primes (+ n 2) (- count 1))))
          (else (search-for-primes (+ n 2) count ))))

(search-for-primes (+ 1000 1) 3)
(search-for-primes (+ 10000 1) 3)
(search-for-primes (+ 100000 1) 3)
(search-for-primes (+ 1000000 1) 3)
(newline)


(display '--------1.23)
(newline)

(define (new-smallest-divisor n) (new-find-divisor n 2))

(define (next n)
    (if (= n 3)
        2
        (+ n 2)))

(define (new-find-divisor n test-divisor) 
    (cond ((> (square test-divisor) n) n)
           ((divides? test-divisor n) test-divisor)
           (else (new-find-divisor n (next test-divisor)))))

(define (new-prime? n)
    (= n (new-smallest-divisor n)))

(define (new-timed-prime-test n)
    (newline)
    (display n)
    (new-start-prime-test n (runtime)))

(define (new-start-prime-test n start-time)
    (if (new-prime? n)
        (report-prime (- (runtime) start-time))))

(define (new-prime? n)
    (= n (new-smallest-divisor n)))

(new-timed-prime-test 1009)
(new-timed-prime-test 1013)
(new-timed-prime-test 1019)
(new-timed-prime-test 10007)
(new-timed-prime-test 10009)
(new-timed-prime-test 10037)
(new-timed-prime-test 100003)
(new-timed-prime-test 100019)
(new-timed-prime-test 100043)
(new-timed-prime-test 1000003)
(new-timed-prime-test 1000033)
(new-timed-prime-test 1000037)
(newline)

(display '--------1.25)
(newline)

(writeln "原始的 expmod 只需要进行多步的比较小的数值的计算，避免了计算 base 的 exp 次方，这个数值可能很大") 

(display '--------1.26)
(newline)

(writeln "* 的参数计算了两遍，函数的参数会先求值，然后再调用函数") 

(display '--------1.27)
(newline)

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m)) 
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (carmichael n a)
    (cond ((= a n) #t)
          ((not (= (expmod a n n) a)) #f)
          (else (carmichael n (+ a 1)))))

(writeln (carmichael 561 1))
(writeln (carmichael 1105 1))
(writeln (carmichael 1729 1))
(writeln (carmichael 562 1))
(writeln (carmichael 100 1))
(writeln (carmichael 19 1))

(display '--------1.28)
(newline)

(define (check x m)
    (if (and (not (= x 1)) (not (= x (- m 1))) (= 1 (remainder (square x) m)))
        0
        (remainder (square x) m)))

(define (mr-expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (check (mr-expmod base (/ exp 2) m) m))
    (else (remainder (* base (mr-expmod base (- exp 1) m)) m))))

(define (mr-test n)
    (define (try-it a)
        (= 1 (mr-expmod a (- n 1) n)))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((mr-test n) (fast-prime? n (- times 1)))
        (else false)))

(writeln (fast-prime? 561 100))
(writeln (fast-prime? 19 100))
(writeln (fast-prime? 2 10))
(writeln (fast-prime? 1105 100))

(display '--------1.29)
(newline)

(define (cube n) (* n n n))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
                (sum term (next a) next b))))

(define (Simpson f a b n)
    (define (re h)
        (* (/ h 3) (sum (lambda (k)
                        (* (cond ((or (= k 0) (= k 1)) 1)
                                 ((odd? k) 4)
                                 (else 2))
                            (f (+ a (* h k))))) a add1 n)))
    (re (/ (- b a) n)))
    
(writeln (Simpson cube 0 1 100))
(writeln (Simpson cube 0 1 1000))

(display '--------1.30)
(newline)

(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))))
    (iter a 0))

(writeln (sum-iter + 0 add1 100))

(display '--------1.31.a)
(newline)

(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

(define (factorial n) (product identity  1 add1 n))

(writeln (factorial 5))

(define (pi n) (* 4 (product (lambda (x) (if (odd? x)
                                             (/ (- x 1) x)
                                             (/ x (- x 1)))) 3 add1 n)))

(writeln (pi 10))
(writeln (pi 50))

(display '--------1.31.b)
(newline)

(define (product-recur term a next b)
    (if (> a b)
        1
        (* (term a)
                (product-recur term (next a) next b))))

(define (factorial-recur n) (product-recur identity  1 add1 n))

(writeln (factorial-recur 5))

(display '--------1.32.a)
(newline)

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(writeln (accumulate + 0 identity 1 add1 100))

(display '--------1.32.b)
(newline)

(define (accumulate-recur combiner null-value term a next b)
    (if (> a b)
        null-value 
        (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

(writeln (accumulate-recur + 0 identity 1 add1 100))

(display '--------1.33.a)
(newline)

(define (filtered-accumulate combiner filter null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner ((lambda (x)
                                        (if (filter x)
                                            x
                                            null-value)) (term a)) result))))
    (iter a null-value))

(writeln (filtered-accumulate + prime? 0 identity 2 add1 10))

(display '--------1.33.b)
(newline)

(define (gcd a b) (if (= b 0)
      a
      (gcd b (remainder a b))))

(writeln (filtered-accumulate * (lambda (x) (= 1 (gcd x 10))) 1 identity 1 add1 10))

(display '--------1.35)
(newline)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(writeln (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))

(display '--------1.36)
(newline)

(define (fixed-point-seq f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (writeln next)
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(writeln (fixed-point-seq (lambda (x) (/ (log 1000) (log x))) 2))

(display '--------1.37.a)
(newline)

(define (cont-frac n d k)
    (if (= k 0)
        0
        (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(writeln (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    20))

(display '--------1.37.b)
(newline)

(define (cont-frac-iter n d k)
    (define (iter r c)
        (if (= c 0)
            r
            (iter (/ (n c) (+ r (d c))) (- c 1))))
    (iter 0 k))

(writeln (cont-frac-iter (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         20))

(display '--------1.38)
(newline)

(define (euler-d i)
    (if (= (remainder i 3) 2)
        (* 2 (+ 1 (quotient i 3)))
        1))

(define (euler-d-test n i)
    (if (= i n)
        '()
        (cons (euler-d i) (euler-d-test n (+ i 1)))))

(writeln (euler-d-test 20 1))

(define (euler n) (+ 2 (cont-frac-iter (lambda (i) 1.0)
                                       euler-d
                                       n)))

(writeln (euler 20))

(display '--------1.39)
(newline)

(define (tan-cf x k)
    (define (iter r c)
        (if (= c 0)
            r
            (iter (/ (square x) (- (* 2 c) 1 r)) (- c 1))))
    (/ x (iter 0 k)))

(writeln (tan-cf 20 10))

(display '--------1.40)
(newline)

(define dx 0.00001)

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) (fixed-point (newton-transform g) guess))

(define (cubic a b c)
    (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(writeln (newtons-method (cubic 1 2 3) 1))

(display '--------1.41)
(newline)

(define (double f)
    (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(writeln (((double (double double)) inc) 5)) ; 21   5+16

(display '--------1.42)
(newline)

(define (compose f g)
    (lambda (x) (f (g x))))

(writeln ((compose square inc) 6))

(display '--------1.43)
(newline)

(define (repeated f n)
    (define (iter c)
        (if (= c 0)
            identity
            (compose f (iter (- c 1)))))
    (lambda (x) ((iter n) x)))

(writeln ((repeated square 2) 5))

(display '--------1.44)
(newline)

(define (smooth f)
    (lambda (x)
        (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (n-fold-smooth f n) (repeated (smooth f) n))

(writeln ((n-fold-smooth square 3) 10))

(display '--------1.44)
(newline)

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (get-max-pow n) 
   (define (iter p r) 
     (if (< (- n r) 0) 
         (- p 1) 
         (iter (+ p 1) (* r 2)))) 
   (iter 1 2)) 

(define (nth-root x n)
    (fixed-point ((repeated average-damp (get-max-pow n)) (lambda (y) (/ x (expt y (- n 1))))) 0.1))

(writeln (nth-root 3 4))

(writeln (nth-root 8 5))

(display '--------1.46)
(newline)

(define (iterative-improve good? improve)
    (define (iter x)
        (if (good? x (improve x))
            (improve x)
            (iter (improve x))))
    (lambda (y) (iter y)))

(define (sqrt-ii x) ((iterative-improve (lambda (y next) (good-enough? next x)) (lambda (y) (average y (/ x y)))) 0.1))

(writeln (sqrt-ii 2))

(define (fixed-point-ii f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    ((iterative-improve close-enough? f) first-guess))

(writeln (fixed-point-ii (lambda (x) (+ 1 (/ 1 x))) 1))

(exit)
