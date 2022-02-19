(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

(display '--------3.14)
(newline)

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))

(define v (list 'a 'b 'c 'd))

(writeln (mystery v))

(display '--------3.17)
(newline)

(define (count-pairs-naive x)
    (if (not (pair? x))
        0
        (+ (count-pairs-naive (car x))
            (count-pairs-naive (cdr x))
            1)))

(writeln (count-pairs-naive '(1 2 3)))
(writeln (count-pairs-naive '(1 2 3 4)))
(define x '(1 2 3 4))
(set-cdr! (cdddr x) x)

; (writeln (count-pairs-naive x))


(display '--------3.17)
(newline)

(define (count-pairs x) 
    (let ((remember '())) 
        (define (helper x) 
            (if (or (not (pair? x)) (memq x remember)) 
                0 
                (begin (set! remember (cons x remember)) 
                       (+ (helper (car x)) (helper (cdr x)) 1)))) 
  (helper x))) 

(writeln (count-pairs x))

(display '--------3.18)
(newline)

(define (has-cycle? x) 
    (let ((remember '())) 
        (define (helper x) 
            (cond ((null? x) #f)
                  ((memq x remember) #t)
                  (else (begin (set! remember (cons x remember)) 
                               (helper (cdr x)))))) 
  (helper x))) 

(writeln (has-cycle? '(1 2 3)))
(writeln (has-cycle? x))

(display '--------3.19)
(newline)

(define (has-cycle-trick? x)
    (define (helper y)
        (cond ((null? y) #f)
              ((eq? x y) #t)
              (else (helper (cdr y)))))
    (helper x))

(writeln (has-cycle? '(1 2 3)))
(writeln (has-cycle? x))

(display '--------3.21)
(newline)

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
    (set-car! queue item))

(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue) (set-front-ptr! queue new-pair) (set-rear-ptr! queue new-pair) queue)
               (else (set-cdr! (rear-ptr queue) new-pair) (set-rear-ptr! queue new-pair) queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
          (else (set-front-ptr! queue (cdr (front-ptr queue))) queue)))

 (define (print-queue q) (map writeln (front-ptr q)))

(define q1 (make-queue))

(print-queue q1)

(insert-queue! q1 1)
(insert-queue! q1 2)
(insert-queue! q1 3)

(print-queue q1)

(display '--------3.28)
(newline)

(define (logic-or s1 s2) 
   (if (or (= s1 1) (= s2 1)) 
         1 
         0)) 

(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
        (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


(exit)