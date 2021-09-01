(define writeln (lambda x (for-each display x) (newline)))

(define compose (lambda (f g)
    (lambda (x)
        (f (g x)))))


(display '--------8.1)
(newline)

(define both
    (lambda (pred)
        (lambda (arg1 arg2)
            (and (pred arg1) (pred arg2)))))

(define (neither-both pred)
    (both (compose not pred)))

(writeln ((neither-both odd?) 0 2))
(writeln ((neither-both odd?) 0 1))
(newline)

(define (at-least-one-both pred)
    (lambda (x y) 
        (not ((both (compose not pred)) x y))))

(writeln ((at-least-one-both odd?) 0 2))
(writeln ((at-least-one-both odd?) 0 1))


(display '--------8.2)
(newline)

(define at-least-one (lambda (pred)
    (lambda (arg1 arg2)
        (or (pred arg1) (pred arg2)))))

(define (neither-least-one pred)
    (lambda (arg1 arg2)
        (not ((at-least-one pred) arg1 arg2))))

(writeln ((neither-least-one odd?) 0 1))
(writeln ((neither-least-one odd?) 0 2))
(writeln ((neither-least-one odd?) 1 3))
(newline)

(define (both-least-one pred)
    (lambda (arg1 arg2)
        (not ((at-least-one (compose not pred)) arg1 arg2))))

(writeln ((both-least-one odd?) 1 3))
(writeln ((both-least-one odd?) 0 3))
(writeln ((both-least-one odd?) 0 2))

(display '--------8.3)
(newline)

(define (equal? a b)
    (if ((neither-both pair?) a b)
        (eqv? a b)
        (if ((both pair?) a b)
            (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))))

(writeln (equal? '(a (b c (d e) f)) ' (a (b c (d e) f))))
(writeln (equal? '(a ((b d) c) e) ' (a (b d) c e)))
(writeln (equal? '(a ((b d) c) e) '(a ((d b) c) e)))


(display '--------8.4)
(newline)

; basic operator of set
(define set-tag "set")
(define the-empty-set (cons set-tag '()))

(define remove
    (lambda (item ls)
        (cond
            ((null? ls) '())
            ((equal? (car ls) item) (remove item (cdr ls)))
            (else (cons (car ls) (remove item (cdr ls)))))))

(define empty-set? (lambda (s)
    (eq? s the-empty-set)))

(define set? (lambda (arg)
    (and (pair? arg) (eq? (car arg) set-tag))))

(define pick
    (lambda (s)
        (let ((ls (cdr s)))
            (if (null? ls)
                (error "pick: The set is empty.")
                (list-ref ls (random (length ls)))))))

(define residue
    (lambda (elem)
        (lambda (s)
            (let ((ls (remove elem (cdr s))))
                (cond
                    ((null? ls) the-empty-set)
                    (else (cons set-tag ls)))))))

(define adjoin
    (lambda (elem s)
        (cons set-tag (cons elem (cdr s)))))

(define make-set (lambda args
    (letrec
        ((list-make-set
            (lambda (args-list)
                (if (null? args-list)
                    the-empty-set
                    (adjoin
                        (car args-list)
                        (list-make-set (cdr args-list)))))))
        (list-make-set args))))


(define none (lambda (pred)
    (letrec
        ((test
            (lambda (s)
                (or (empty-set? s)
                    (let ((elem (pick s)))
                        (and (not (pred elem))
                            (test ((residue elem) s))))))))
        test)))

(define there-exists (lambda (pred)
    (letrec
        ((test
            (lambda (s)
                (if (empty-set? s)
                    #f
                    (let ((elem (pick s)))
                        (or (pred elem)
                            (test ((residue elem) s))))))))
        test)))

(define (for-all-there-exists pred)
    (lambda (ls)
        (not ((there-exists (compose not pred)) ls))))

(writeln ((for-all-there-exists odd?) (make-set 1 3)))
(writeln ((for-all-there-exists odd?) (make-set 1 2)))
(writeln ((for-all-there-exists odd?) (make-set 2 4)))
(newline)

(define (none-there-exists pred)
    (lambda (ls)
        (not ((there-exists pred) ls))))

(writeln ((none-there-exists odd?) (make-set 1 3)))
(writeln ((none-there-exists odd?) (make-set 1 2)))
(writeln ((none-there-exists odd?) (make-set 2 4)))
(newline)

(define for-all (lambda (pred)
    (letrec
        ((test
            (lambda (s)
                (if (empty-set? s)
                    #t
                    (let ((elem (pick s)))
                        (and (pred elem)
                             (test ((residue elem) s))))))))
        test)))

(writeln ((for-all odd?) '(1 3)))
(writeln ((for-all odd?) '(1 2)))
(writeln ((for-all odd?) '(2 4)))
(newline)

(define (none-for-all pred)
    (for-all (compose not pred)))

(writeln ((none-for-all odd?) (make-set 1 3)))
(writeln ((none-for-all odd?) (make-set 1 2)))
(writeln ((none-for-all odd?) (make-set 2 4)))
(newline)

(define (there-exists-for-all pred)
    (lambda (x)
        (not ((for-all (compose not pred)) x))))

(writeln ((there-exists-for-all odd?) (make-set 1 3)))
(writeln ((there-exists-for-all odd?) (make-set 1 2)))
(writeln ((there-exists-for-all odd?) (make-set 2 4)))
(newline)

(display '--------8.5)
(newline)

(define for-one
    (lambda (pred found-proc not-found-proc)
        (letrec ((test
            (lambda (s)
                (if (empty-set? s)
                    (not-found-proc)
                    (let ((v (pick s)))
                        (if (pred v)
                            (found-proc v)
                            (test ((residue v) s) )))))))
            test)))

(define (there-exists-for-one pred)
    (for-one pred (lambda (v) #t) (lambda () #f)))

(writeln ((there-exists-for-one odd?) (make-set 1 3)))
(writeln ((there-exists-for-one odd?) (make-set 1 2)))
(writeln ((there-exists-for-one odd?) (make-set 2 4)))
(newline)

(define (for-all-for-one pred)
    (lambda (x)
        (not ((for-one (compose not pred) (lambda (v) #t) (lambda () #f)) x))))

(writeln ((for-all-for-one odd?) '(1 3)))
(writeln ((for-all-for-one odd?) '(1 2)))
(writeln ((for-all-for-one odd?) '(2 4)))
(newline)

(exit)