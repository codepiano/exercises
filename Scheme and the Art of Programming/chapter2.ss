(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

; 2.1
(display '--------2.1)
(newline)

(define (second data)
    (car (cdr data)))

(display (second '(1 2)))
(newline)
(display (second '(1 (2 3))))
(newline)
(display (second '((0 1) (2 3) 4)))
(newline)
(newline)

; 2.2
(display '--------2.2)
(newline)

(define (third data)
    (car (cdr (cdr data))))

(display (third '(1 2 3)))
(newline)
(display (third '(1 (2 3) 4)))
(newline)
(display (third '((0 1) (2 3) 4 5)))
(newline)
(newline)

; 2.4
(display '--------2.4)
(newline)

(define (juggle data)
    (cons (third data) (cons (second data) (cons (car data) '()))))

(display (juggle '(jump quick spot)))
(newline)
(display (juggle '(dog bites mEm) ))
(newline)
(newline)

; 2.5
(display '--------2.5)
(newline)

(define (switch data)
    (cons (car data) (cons (third data) '())))

(display (switch '(jump quick spot)))
(newline)
(display (switch '(dog bites mEm) ))
(newline)
(newline)

; 2.10
(display '--------2.10)
(newline)

(define (last-item data)
    (if (null? (cdr data))
        (car data)
        (last-item (cdr data))))

(display (last-item '(1 2 3 4)))
(newline)
(newline)

(define (member? item data)
    (if (null? data)
        #f
        (or (equal? item (car data)) (member? item (cdr data)))))

(display (member? 0 '(1 2 3 4)))
(newline)
(display (member? 1 '(1 2 3 4)))
(newline)
(newline)

(define (remove-1st item data)
    (if (null? data)
        '()
        (if (equal? item (car data))
            (cdr data)
            (cons (car data) (remove-1st item (cdr data))))))

(display (remove-1st 1 '(1 2 3)))
(newline)
(display (remove-1st 2 '(1 2 3)))
(newline)
(display (remove-1st 4 '(1 2 3)))
(newline)
(newline)

(define (remove-1st-another item data)
    (if (null? data)
        '()
        (or (and (equal? item (car data)) (cdr data))
            (cons (car data) (remove-1st-another item (cdr data))))))

(display (remove-1st-another 1 '(1 2 3)))
(newline)
(display (remove-1st-another 2 '(1 2 3)))
(newline)
(display (remove-1st-another 4 '(1 2 3)))
(newline)
(newline)

; 2.11
(display '--------2.11)
(newline)

(define (member?-another item data)
    (cond ((null? data) #f)
          ((equal? item (car data)))
          (else (member? item (cdr data)))))

(display (member?-another 0 '(1 2 3 4)))
(newline)
(display (member?-another 1 '(1 2 3 4)))
(newline)
(display (member?-another '(1 2) '(1 2 3 4 (1 2))))
(newline)
(newline)

; 2.12
(display '--------2.12)
(newline)

(define (remove-last-one ls)
    (if (null? (cddr ls))
        (cons (car ls) '())
        (cons (car ls) (remove-last-one (cdr ls)))))

(display (remove-last-one '(1 2 3 4 5)))
(newline)
(newline)

; 2.13
(display '--------2.13)
(newline)

(define (subst-1st new old ls)
    (cond ((null? ls) '())
          ((equal? old (car ls)) (cons new (cdr ls)))
          (else (cons (car ls) (subst-1st new old (cdr ls))))))

(display (subst-1st 'new 'old '(new old)))
(newline)
(display (subst-1st 'new 'old '(new new)))
(newline)
(display (subst-1st 'new 'old '(old old)))
(newline)
(display (subst-1st 'dog 'cat '(my cat is clever)))
(newline)
(display (subst-1st 'b 'a ' (c a b a c)))
(newline)
(display (subst-1st '(0) '() '((*) (1) () (2))))
(newline)
(display (subst-1st 'two 'one '()))
(newline)

(define (substq-1st new old ls)
    (cond ((null? ls) '())
          ((eq? old (car ls)) (cons new (cdr ls)))
          (else (cons (car ls) (subst-1st new old (cdr ls))))))

(define (substv-1st new old ls)
    (cond ((null? ls) '())
          ((eqv? old (car ls)) (cons new (cdr ls)))
          (else (cons (car ls) (subst-1st new old (cdr ls))))))

(newline)

; 2.14
(display '--------2.14)
(newline)

(define (insert-right-lst new old ls)
    (cond ((null? ls) '())
          ((equal? (car ls) old) (cons old (cons new (cdr ls))))
          (else (cons (car ls) (insert-right-lst new old (cdr ls))))))

(display (insert-right-lst 'not 'does '(my dog does have fleas)))
(newline)

(define (insert-left-lst new old ls)
    (cond ((null? ls) '())
          ((equal? (car ls) old) (cons new (cons old (cdr ls))))
          (else (cons (car ls) (insert-left-lst new old (cdr ls))))))

(display (insert-left-lst 'hot 'dogs '(I eat dogs)))
(newline)
(display (insert-left-lst 'fun 'games '(some fun)))
(newline)
(display (insert-left-lst 'a 'b '(a b c a b c)))
(newline)
(display (insert-left-lst 'a 'b '()) )
(newline)
(newline)

; 2.15
(display '--------2.15)
(newline)

(define (list-of-first-items data)
    (if (null? data)
        '()
        (cons (caar data) (list-of-first-items (cdr data)))))

(display (list-of-first-items '((a) (b c d) (e f))))
(newline)
(display (list-of-first-items '((1 2 3) (4 5 6))))
(newline)
(display (list-of-first-items '((one))))
(newline)
(display (list-of-first-items '()))
(newline)
(newline)

; 2.16
(display '--------2.16)
(newline)

(define (replace new data)
    (if (null? data)
        '()
        (cons new (replace new (cdr data)))))

(display (replace 'no '(sill you do ae a favor)))
(newline)
(display (replace 'yes '(do you like ice creaa)))
(newline)
(display (replace 'shy '(not)))
(newline)
(display (replace 'maybe '()))
(newline)
(newline)

; 2.17
(display '--------2.17)
(newline)

(define (remove-2nd item data)
    (if (null? data)
        '()
        (if (equal? item (car data))
            (cons item (remove-1st item (cdr data)))
            (cons (car data) (remove-2nd item (cdr data))))))

(display (remove-2nd 'cat '(my cat loves cat food)))
(newline)
(display (remove-2nd 'cat '(my cat loves food)))
(newline)
(display (remove-2nd 'cat '(my cat and your cat love cat food)))
(newline)
(display (remove-2nd 'cat '()))
(newline)
(newline)

; 2.18
(display '--------2.18)
(newline)

(define (remove-last item data)
    (cond ((null? data) '())
          ((and (equal? item (car data)) (not (member item (cdr data)))) (cdr data))
          (else (cons (car data) (remove-last item (cdr data))))))

(display (remove-last 'a '(b a n a n a s)))
(newline)
(display (remove-last 'a '(b a n a n a )))
(newline)
(display (remove-last 'a '()))
(newline)
(newline)

; 2.19
(display '--------2.19)
(newline)

(define (sandwich-1st a b ls)
    (cond ((null? ls) '())
          ((null? (cdr ls)) ls)
          ((and (equal? b (car ls)) (equal? b (cadr ls)))  (cons b (cons a (cdr ls))))
          (else (cons (car ls) (sandwich-1st a b (cdr ls))))))

(display (sandwich-1st 'meat 'bread '(bread cheese bread bread)))
(newline)
(display (sandwich-1st 'meat 'bread '(bread jam bread cheese bread)))
(newline)
(display (sandwich-1st 'meat 'bread '()))
(newline)
(newline)

; 2.20
(display '--------2.20)
(newline)

(define (list-of-symbols?-cond data)
    (cond ((null? data) #t)
          ((symbol? (car data)) (list-of-symbols?-cond (cdr data)))
          (else #f)))

(display (list-of-symbols?-cond '(one two three four five)))
(newline)
(display (list-of-symbols?-cond '(cat dog (hen pig) cow)))
(newline)
(display (list-of-symbols?-cond ' (a b 3 4 d) ))
(newline)
(display (list-of-symbols?-cond '()))
(newline)
(newline)

(define (list-of-symbols?-if data)
    (if (null? data)
        #t
        (if (symbol? (car data))
            (list-of-symbols?-if (cdr data))
            #f)))
(display (list-of-symbols?-if '(one two three four five)))
(newline)
(display (list-of-symbols?-if '(cat dog (hen pig) cow)))
(newline)
(display (list-of-symbols?-if ' (a b 3 4 d) ))
(newline)
(display (list-of-symbols?-if '()))
(newline)
(newline)

(define (list-of-symbols?-bool data)
    (or (null? data)
        (and (symbol? (car data)) (list-of-symbols?-bool (cdr data)))))

(display (list-of-symbols?-bool '(one two three four five)))
(newline)
(display (list-of-symbols?-bool '(cat dog (hen pig) cow)))
(newline)
(display (list-of-symbols?-bool ' (a b 3 4 d) ))
(newline)
(display (list-of-symbols?-bool '()))
(newline)
(newline)

; 2.21
(display '--------2.21)
(newline)

(define (all-same? data)
    (or (null? data)
        (null? (cdr data))
        (and (equal? (car data) (cadr data)) (all-same? (cdr data)))))

(display (all-same? '(a a a a a)))
(newline)
(display (all-same? '(a b a b a b)))
(newline)
(display (all-same? '((a b) (a b) (a b))))
(newline)
(display (all-same? '(a)))
(newline)
(display (all-same? '()))
(newline)
(newline)

; 2.25
(display '--------2.25)
(newline)

(define writeln (lambda x (for-each display x) (newline)))

(define entering
    (lambda (test input cond-clause-number)
        (begin
            (if test (writeln "Entering cond-clause-" cond-clause-number " with ls = " input))
            test)))

(define leaving
    (lambda (result cond-clause-number)
        (begin
            (writeln "Leaving cond-clause-" cond-clause-number " with result = " result)
            result)))

(define swapper-trace
    (lambda (x y ls)
        (cond ((entering (null? ls) ls 1) (leaving '() 1))
              ((entering (equal? (car ls) x) ls 2) (leaving (cons y (swapper-trace x y (cdr ls))) 2))
              ((entering (equal? (car ls) y) ls 3) (leaving (cons x (swapper-trace x y (cdr ls))) 2))
              ((entering 'else ls 4) (leaving (cons (car ls) (swapper-trace x y (cdr ls))) 4)))))

(display (swapper-trace 'b 'd '(a b c b d)))
(newline)
(newline)

; 2.27
(display '--------2.27)
(newline)

(define tracing
    (lambda (message result)
        (begin
            (writeln message result)
            result)))

(define test-tracing
    (lambda (test message input)
        (begin
            (if test (tracing message input))
            test)))

(define remove-lst-trace
    (lambda (item ls)
        (cond ((test-tracing (null? ls) "Entering cond-clause-1 with ls = " ls) (tracing "Leaving cond-clause-1 with result = " '()))
              ((test-tracing (equal? (car ls) item) "Entering cond-clause-2 with ls = " ls) (tracing "Leaving cond-clause-2 with result =" (cdr ls)))
              ((test-tracing 'else "Entering cond-clause-3 with ls = " ls) (tracing "Leaving cond-clause-3 with result = " (cons (car ls) (remove-lst-trace item (cdr ls))))))))

(display (remove-lst-trace 'c '(a b c d)))
(newline)
(exit)