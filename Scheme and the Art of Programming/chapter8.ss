(define writeln (lambda x (for-each display x) (newline)))

(define compose (lambda (f g)
    (lambda (x)
        (f (g x)))))

(define (add1 n)
    (+ n 1))

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
            (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
            #f)))

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

(define contains (lambda (set)
    (lambda (obj)
        ((element obj) set))))

(define neither
    (lambda (pred)
        (lambda (arg1 arg2)
            (not (or (pred arg1) (pred arg2))))))

(define superset
    (lambda (s1)
        (lambda (s2)
            ((for-all (contains s1)) s2))))

(define subset (lambda (s1)
    (lambda (s2)
        ((superset s2) s1))))

(define set-equal
    (lambda (obj1)
        (lambda (obj2)
            (or (and ((neither set?) obj1 obj2)
                     (equal? obj1 obj2))
                (and ((both set?) obj1 obj2)
                     ((subset obj1) obj2)
                     ((subset obj2) obj1))))))


(define element (compose there-exists set-equal))

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
                                       (else (letrec ((s1 (car args))
                                                      (s2 (cadr args)))
                                                     (foldLeft (cons (helper s1 s2) (cddr args)))))))))
            (foldLeft args))))

(writeln (intersection (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 1 5 6 3 7)))
(writeln (intersection (make-set 1 3 4 5) the-empty-set (make-set 1 5 6 3 7)))
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
                                       (else (letrec ((s1 (car args))
                                                      (s2 (cadr args)))
                                                     (foldLeft (cons (helper s1 s2) (cddr args)))))))))
            (foldLeft args))))

(writeln (union (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 2 1)))
(writeln (union (make-set 1 2 3 4) (make-set 1 3 4 5) the-empty-set (make-set 2 1)))
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
                                    (else (letrec ((s1 (car args))
                                                    (s2 (cadr args)))
                                                    (foldLeft (cons (helper s1 s2) (cddr args)))))))))
            foldLeft))

(define intersection-flat (lambda args ((flat-set
                                            (lambda (x y) the-empty-set)
                                            (lambda (x) x)) args)))

(writeln (intersection-flat (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 1 5 6 3 7)))
(writeln (intersection-flat (make-set 1 3 4 5) the-empty-set (make-set 1 5 6 3 7)))
(newline)

(define union-flat (lambda args ((flat-set
                                    (lambda (x y) y)
                                    (lambda (x) (not x))) args)))

(writeln (union-flat (make-set 1 2 3 4) (make-set 1 3 4 5) (make-set 2 1)))
(writeln (union-flat (make-set 1 2 3 4) (make-set 1 3 4 5) the-empty-set (make-set 2 1)))
(newline)

(display '--------8.9)
(newline)

(define set-builder
    (lambda (pred base-set)
        (letrec
            ((helper
                (lambda (s)
                    (if (empty-set? s)
                        base-set
                        (let ((elem (pick s)))
                            (if (pred elem)
                                (adjoin elem (helper ((residue elem) s)))
                                (helper ((residue elem) s))))))))
            helper)))

(define difference (lambda (s1 s2)
    ((set-builder (compose not (contains s2)) the-empty-set) s1)))

(define (symmetric-difference s1 s2)
    (letrec ((i1 (difference s1 s2))
            (i2 (difference s2 s1)))
            (union i1 i2)))

(writeln (symmetric-difference (make-set 1 2 3 4 5) (make-set 3 4 5 6 7)))
(newline)

(display '--------8.10)
(newline)

(define set-equal?
    (lambda (obj1 obj2)
        ((set-equal obj1) obj2)))

(define adjoin-c
    (lambda (elem)
        (lambda (s)
           (cons set-tag (cons elem (cdr s))))))

(define set-map
    (lambda (proc s)
        (if (empty-set? s)
            the-empty-set
            (let ((elem (pick s)))
                 (adjoin (proc elem)
                    (set-map proc ((residue elem) s)))))))

(define (power-set s)
    (letrec ((generate (lambda (x y)
                        (let ((n (set-map (adjoin-c x) y)))
                            (union n y))))
             (helper (lambda (l)
                        (if (empty-set? l)
                            (make-set the-empty-set)
                            (let ((elem (pick l)))
                                (generate elem (helper ((residue elem) l))))))))
        (helper s)))

(writeln (power-set (make-set 'a 'b 'c)))
(newline)

(display '--------8.11)
(newline)

(define cardinal
    (lambda (s)
        (if (empty-set? s)
        0
        (let ((elem (pick s)))
            (add1 (cardinal ((residue elem) s)))))))

(define (select-by-cardinal n)
    (lambda (s)
        (if (empty-set? s)
            the-empty-set
            (let ((elem (pick s)))
                (if (= n (cardinal elem))
                    (adjoin elem ((select-by-cardinal n) ((residue elem) s)))
                    ((select-by-cardinal n) ((residue elem) s)))))))

(writeln ((select-by-cardinal 2) (make-set (make-set 'a) (make-set 'a 'b) (make-set 'a 'b 'c) (make-set 'b 'c) (make-set 'b))))
(newline)

(display '--------8.12)
(newline)

(define make-op-list list)
(define op?-list (lambda (ls) (and (pair? ls) (pair? (cdr ls)) (null? (cddr ls)))))
(define op-1st-list car)
(define op-2nd-list cadr)

(define make-op cons)
(define op? pair?)
(define op-1st car)
(define op-2nd cdr)

(display '--------8.13)
(newline)

(define (relation? s)
    ((for-all op?) s))

(writeln (relation? (make-set (make-op 'a 0) (make-op 'a 1) (make-op 'c 1))))
(writeln (relation? (make-set 1 (make-op 'a 1) (make-op 'c 1))))
(newline)

(display '--------8.14)
(newline)

(define (inverse-pair p)
    (if (op? p)
        (make-op (op-2nd p) (op-1st p))
        #f))

(define (inverse-relation s)
    (set-map inverse-pair s))

(writeln (inverse-relation (make-set (make-op 'a 0) (make-op 'a 1) (make-op 'c 1))))
(newline)

(display '--------8.15)
(newline)

(define subrelation/1st
    (lambda (rel)
        (lambda (arg)
            ((set-builder
                (lambda (x) ((set-equal (op-1st x)) arg))
                the-empty-set) rel))))

(define function?
    (lambda (rel)
        (or (empty-set? rel)
            (let ((subrel ((subrelation/1st rel) (op-1st (pick rel)))))
                (and (= (cardinal subrel) 1)
                    (function? (difference rel subrel)))))))

(define (one-to-one? s)
    (function? (inverse-relation s)))

(writeln (one-to-one? (make-set (make-op 'a 0) (make-op 'a 1) (make-op 'c 1))))
(writeln (one-to-one? (make-set (make-op 'a 0) (make-op 'a 1) (make-op 'c 2))))
(newline)

(display '--------8.16)
(newline)

(define list->set (lambda (ls)
    (apply make-set ls)))

(define make-relation
    (lambda args
        (if (null? args)
            the-empty-set
            (if ((for-all op?) args)
                (list->set (map (lambda (x) (make-op (car x) (cadr x))) args)) 
                the-empty-set))))


(writeln (make-relation '(1 2) '(1 3) '(2 3)))
(newline)

(display '--------8.17)
(newline)

(define cartesian-product
    (lambda (s1 s2)
        (if (empty-set? s1)
            the-empty-set
            (let ((elem (pick s1)))
                (union (set-map (lambda (x) (make-op elem x)) s2)
                        (cartesian-product ((residue elem) s1) s2))))))

(define (reflexive? r)
    (let ((s (set-map op-1st r))
          (c (contains r)))
        ((for-all (lambda (x)
                    (if (c (make-op x x))
                        #t
                        #f))) s)))

(writeln (reflexive? (cartesian-product (make-set 1 2 3) (make-set 1 2 3))))
(writeln (reflexive? (make-relation '(1 1) '(1 2) '(1 3) '(2 1) '(2 2) '(2 3) '(3 1) '(3 2))))

(display '--------8.18)
(newline)

(define (symmetric? r)
    (set-equal? r (inverse-relation r)))

(writeln (symmetric? (cartesian-product (make-set 1 2 3) (make-set 1 2 3))))
(writeln (symmetric? (make-relation '(1 1) '(1 2) '(1 3) '(2 1) '(2 2) '(2 3) '(3 1))))

(display '--------8.19)
(newline)

(define domain
    (lambda (rel)
        (set-map op-1st rel)))

(define range
    (lambda (rel)
        (set-map op-2nd rel)))

(define family-union 
    (lambda (s)
        (if (empty-set? s)
            the-empty-set
            (let ((elem (pick s)))
                (union elem (family-union ((residue elem) s)))))))

(define value
    (lambda (fun)
        (lambda (arg)
            (op-2nd (pick ((subrelation/1st fun) arg))))))

(define (function-compose f g)
    (letrec ((gr (range g))
             (gd (domain g))
             (fd (domain f))
             (vg (value g))
             (s1 (subrelation/1st f)))
             (if (not ((subset gr) fd))
                 the-empty-set
                 (letrec ((gf (family-union
                                (set-map (lambda (x)
                                    (cartesian-product (make-set x)
                                                       (set-map op-2nd (s1 (vg x))))) gd))))
                    gf))))

(writeln (function-compose (make-relation '(10 100) '(11 101) '(12 102) '(13 100) '(14 104)) 
                           (make-relation '(1 10) '(2 11) '(3 10) '(4 12) '(5 13))))
(newline)

(display '--------8.20)
(newline)

(define (relation-compose f g)
    (letrec ((gr (range g))
             (gd (domain g))
             (fd (domain f))
             (vg (value g))
             (subf (subrelation/1st f))
             (subg (subrelation/1st g))
             (helper (lambda (x)
                        (family-union (set-map subf (set-map op-2nd (subg x)))))))
             (if (not ((subset gr) fd))
                 the-empty-set
                 (letrec ((gf (family-union
                                (set-map (lambda (x)
                                    (cartesian-product (make-set x)
                                                       (set-map op-2nd (helper x)))) gd))))
                    gf))))

(writeln (relation-compose (make-relation '(2 1) '(3 1) '(1 3) '(2 2) '(3 3)) 
                           (make-relation '(1 2) '(2 3) '(2 2) '(3 2) '(1 1))))
(newline)

(display '--------8.21)
(newline)

(define (transitive? r)
    ((subset (relation-compose r r)) r))


(writeln (transitive? (make-relation '(1 2) '(1 3) '(1 4) '(2 3) '(2 4) '(3 4))))
(writeln (transitive? (make-relation '(0 0) '(1 1) '(2 2) '(3 3) '(4 4))))
(writeln (transitive? (make-relation '(1 1) '(1 2) '(3 2) '(2 1))))
(newline)

(display '--------8.21)
(newline)

(define (equivalence-relation? rel)
    (and (reflexive? rel)
         (symmetric? rel)
         (transitive? rel)))


(writeln (equivalence-relation? (make-relation '(0 0) '(1 1) '(2 2) '(3 3))))
(writeln (equivalence-relation? (make-relation '(0 0) '(0 1) '(1 0) '(1 1))))
(writeln (equivalence-relation? (make-relation '(0 0) '(0 1) '(1 1) '(2 2))))
(newline)

(exit)