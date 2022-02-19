(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

(display '--------2.80)
(newline)

(define (=zero? x) (apply-generic '=zero? x)) 

(put '=zero? 'scheme-number (lambda (x) (= x 0))) 

(put '=zero? 'rational-number  (lambda (x) (= (numer x) 0)))

(put '=zero? 'complex-number (lambda (x) (= (real-part x) (imag-part x) 0))) 

(display '--------2.83)
(newline)

(define (raise x) (apply-generic 'raise x)) 
  
 (put 'raise 'integer  (lambda (x) (make-rational x 1))) 
  
 (put 'raise 'rational (lambda (x) (make-real (/ (numer x) (denom x))))) 
  
 (put 'raise 'real (lambda (x) (make-from-real-imag x 0))) 
  

(exit)