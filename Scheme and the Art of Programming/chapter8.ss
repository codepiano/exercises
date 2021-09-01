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

(display '--------8.8)
(newline)


(define neither
    (lambda (pred)
        (lambda (arg1 arg2)
            (not (or (pred arg1) (pred arg2))))))

(define set-equal
    (lambda (objl)
        (lambda (obj2)
            (or (and ((neither set?) objl obj2)
                     (equal? objl obj2))
                (and ((both set?) objl obj2)
                     ((subset objl) obj2)
                     ((subset obj2) objl))))))

(define element (compose there-exists set-equal))

(define contains (lambda (set)
    (lambda (obj)
        ((element obj) set))))

(define intersection
    (lambda args
        (letrec ((helper (lambda (s1 s2)
                            (if (empty-set? s1)
                                the-empty-set
                                (let ((elem (pick s1)))
                                    (if ((contains s2) elem)
                                        (adjoin elem (helper ((residue elem) s1) s2))
                                        (helper ((residue elem) s1) s2))))))
                 (foldLeft (lambda (args)
                                (cond ((null? args) the-empty-set)
                                       ((empty-set? (car args)) the-empty-set)
                                       ((null? (cdr args)) (car args))
                                       ((empty-set? (cadr args)) the-empty-set)
                                       (else (letrec ((s1 (car args))
                                                      (s2 (cadr args)))
                                                     (foldLeft (cons (helper s1 s2) (cddr args)))))))))
            (foldLeft args))))

(writeln (intersection (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 1 5 6 3 7)))
(newline)

(define union
    (lambda args
        (letrec ((helper (lambda (s1 s2)
                            (if (empty-set? s1)
                                s2
                                (let ((elem (pick s1)))
                                    (if (not ((contains s2) elem))
                                        (adjoin elem (helper ((residue elem) s1) s2))
                                        (helper ((residue elem) s1) s2))))))
                 (foldLeft (lambda (args)
                                (cond ((null? args) the-empty-set)
                                       ((empty-set? (car args)) the-empty-set)
                                       ((null? (cdr args)) (car args))
                                       ((empty-set? (cadr args)) the-empty-set)
                                       (else (letrec ((s1 (car args))
                                                      (s2 (cadr args)))
                                                     (foldLeft (cons (helper s1 s2) (cddr args)))))))))
            (foldLeft args))))

(writeln (union (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 2 1)))
(newline)

(define (flat-set empty containsProc)
    (letrec ((helper (lambda (s1 s2)
                            (if (empty-set? s1)
                                (empty s1 s2)
                                (let ((elem (pick s1)))
                                    (if (containsProc ((contains s2) elem))
                                        (adjoin elem (helper ((residue elem) s1) s2))
                                        (helper ((residue elem) s1) s2))))))
             (foldLeft (lambda (args)
                            (cond ((null? args) the-empty-set)
                                    ((empty-set? (car args)) the-empty-set)
                                    ((null? (cdr args)) (car args))
                                    ((empty-set? (cadr args)) the-empty-set)
                                    (else (letrec ((s1 (car args))
                                                    (s2 (cadr args)))
                                                    (foldLeft (cons (helper s1 s2) (cddr args)))))))))
            foldLeft))

(define intersection-flat (lambda args ((flat-set (lambda (x y) the-empty-set) (lambda (x) x)) args)))

(writeln (intersection-flat (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 1 5 6 3 7)))
(newline)

(define union-flat (lambda args ((flat-set (lambda (x y) y) (lambda (x) (not x))) args)))

(writeln (union-flat (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 2 1)))
(newline)

(exit)