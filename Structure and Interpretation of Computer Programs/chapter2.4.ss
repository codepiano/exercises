(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

(display '--------2.73.b)
(newline)

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp))
                      (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-sum-pakcage)
    (define (sum expr var)
        (make-sum (deriv (addend expr) var) (deriv (augend expr) var)))
    (define (addend x) (car x))
    (define (augend x) (cadr x))
    (define (make-sum x y)
        (cond ((and (number? x) (number? y)) (+ x y))
              ((=number? x 0) y)
              ((=number? y 0) x)
              (else (list '+ x y))))
    (put 'deriv '+ sum))

(display '--------2.75)
(newline)

 (define (make-from-mag-ang magni angle) 
   (define (dispatch op) 
         (cond ((eq? op 'real-part) (* magni (cos angle))) 
               ((eq? op 'imag-part) (* magni (sin angle))) 
               ((eq? op 'magnitude) magni) 
               ((eq? op 'angle) angle) 
               (else (error "Unkown op --- MAKE-FROM-MAG-ANG" op)))) 
   dispatch) 

(exit)