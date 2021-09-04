(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define (merge rel l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((rel (car l1) (car l2)) (cons (car l1) (merge rel (cdr l1) l2)))
          (else (cons (car l2) (merge rel l1 (cdr l2))))))

(display '--------10.1)
(newline)

(define (decr-ints n)
    (letrec ((vec (make-vector n)) 
             (helper (lambda (i)
                            (if (< i n)
                                (begin (vector-set! vec i (- n i))
                                       (helper (add1 i)))
                                vec))))
            (helper 0)))


(writeln (decr-ints 10))

(display '--------10.2)
(newline)

(define make-groups
    (lambda (rel ls)
        (cond
            ((null? ls) '())
            ((null? (cdr ls)) (list ls))
            (else (let ((a (car ls))
                       (gps (make-groups rel (cdr ls))))
                    (if (rel (cadr ls) a)
                        (cons (list a) gps)
                        (cons (cons a (car gps)) (cdr gps))))))))

(define pair-merge
    (lambda (rel sublists)
        (cond
            ((null? sublists) '())
            ((null? (cdr sublists)) sublists)
            (else (cons (merge rel (car sublists) (cadr sublists))
                        (pair-merge rel (cddr sublists)))))))

(define (nat-mergesort rel)
    (lambda (ls)
        (if (null? ls)
            '()
            (letrec ((sort (lambda (gps)
                                (if (null? (cdr gps))
                                    (car gps)
                                    (sort (pair-merge rel gps))))))
                (sort (make-groups rel ls))))))

(writeln ((nat-mergesort <) '(9 9 8 3 4 1 0 4 5 8 2 9 8 7 5 6)))
(writeln ((nat-mergesort string<?) '("Rewrite" "the" "definition" "of" "nat-mergesort" "so" "that" "it" "is" "curried")))

(display '--------10.3)
(newline)

(define table10-17
'(("Smith, Harold W." 2324 43 1974 "Fox, Charles Q." 49325)
  ("Jones, Mary Ann" 1888 54 1965  "none" 65230)
  ("White, Thomas P." 3403 34 1982 "Smith, Harold W." 27300)
  ("Williams, John" 2451 46 1970 "Jones, John Paul" 41050)
  ("Brown, Susan E." 3620 28 1984 "Williams, John" 18500)
  ("August, Elizabeth" 2221 45 1971 "Jones, John Paul" 44100)
  ("Jones, John Paul" 1990 55 1965 "Jones, Maury Ann" 63700)
  ("Wilson, William W." 2455 46 1970 "August, Elizabeth" 41050)
  ("Black, Burton P." 3195 38 1978 "Smith, Harold W." 31420)
  ("Fox, Charles q." 2400 41 1981 "Jones, John Paul" 52200)
  ("Blue, Benjamin J." 3630 26 1984 "Williams, John" 18500)))

(writeln ((nat-mergesort (lambda (x y)
                            (string<? (car x) (car y)))) table10-17))
(newline)

(writeln ((nat-mergesort (lambda (x y)
                            (< (cadr x) (cadr y)))) table10-17))
(newline)

(writeln ((nat-mergesort (lambda (x y)
                            (> (cadr (cddddr x))  (cadr (cddddr y))))) table10-17))
(newline)

(writeln ((nat-mergesort (lambda (x y)
                            (< (cadddr x)  (cadddr y)))) table10-17))

(display '--------10.5)
(newline)

(define (list-linear-search ls n)
    (letrec ((helper (lambda (ls n x)
                        (cond ((null? ls) #f)
                              ((= (car ls) n) x)
                              (else (helper (cdr ls) n (add1 x)))))))
        (helper ls n 0)))

(writeln (list-linear-search '(1 2 3 4 5 6 7 8 9) 8))


(display '--------10.7)
(newline)

(define set-tag "set")
(define the-empty-set (cons set-tag '()))

(define empty-set?
    (lambda (s)
        (eq? s the-empty-set)))

(define pick
    (lambda (s)
        (let ((ls (cdr s)))
            (if (null? ls)
                (error "pick: The set is empty.")
                (list-ref ls (random (length ls)))))))

(define remove
    (lambda (item ls)
        (cond
            ((null? ls) '())
            ((equal? (car ls) item) (remove item (cdr ls)))
            (else (cons (car ls) (remove item (cdr ls)))))))

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

(define unlist
    (lambda (proc)
        (lambda (ls)
            (apply proc ls))))

(define list->set
    (lambda (ls)
        (apply make-set ls)))

(define set10-17 (list->set table10-17))

(define over-45?
    (unlist
        (lambda (name id age yr-emp supervisor salary) (> age 45))))

(define set-builder-map
    (lambda (pred proc s)
        (lambda (l)
            (if (empty-set? l)
                s
                (let ((elem (pick l)))
                    (if (pred elem)
                        (adjoin (proc elem) ((set-builder-map pred proc s) ((residue elem) l)))
                        ((set-builder-map pred proc s) ((residue elem) l))))))))

(writeln ((set-builder-map
                over-45?
                (unlist
                    (lambda (name id age yr-emp supervisor salary)
                        (list id age yr-emp salary)))
                    the-empty-set)
            set10-17))

(display '--------10.8)
(newline)

(define select 
    (lambda (t)
        (let ((pred (unlist (lambda (name id age yr-emp supervisor salary)
                                (and (> age 40)
                                     (< yr-emp 1975)
                                     (> salary 43000))))))
        (if (null? t)
            '()
            (if (pred (car t))
                (cons (car t) (select (cdr t)))
                (select (cdr t)))))))

(writeln (select table10-17))

(display '--------10.9)
(newline)

(define table10-27 '((1888 22300 33000) 
                     (1990 61080 49320)
                     (2221 41000 52200)
                     (2324 25550 31500)
                     (2400 31010 25250)
                     (2451 28800 16500)
                     (2455 72050 50010)
                     (3195 60500 40220)
                     (3403 31100 22500)
                     (3620 31100 22500)
                     (3630 26300 19400)))
                            
(define binary-search
    (lambda (rel proc)
        (lambda (vec target)
            (letrec
                ((search
                    (lambda (left right)
                        (if (< right left)
                            (writeln "The search failed.")
                                (let ((middle (floor (/ (+ left right) 2))))
                                    (let ((mid-val (vector-ref vec middle)))
                                        (cond ((rel target (proc mid-val)) (search left (sub1 middle)))
                                               ((rel (proc mid-val) target) (search (add1 middle) right))
                                               (else (vector-ref vec middle)))))))))
                (search 0 (sub1 (vector-length vec)))))))

(define list->vector
    (lambda (ls)
        (let ((vec (make-vector (length ls))))
            (letrec
                ((convert
                    (lambda (ls i)
                        (if (not (null? ls))
                            (begin
                                (vector-set! vec i (car ls))
                                (convert (cdr ls) (add1 i)))))))
                (convert ls 0))
        vec)))

(define (join l)
    (let ((table (list->vector table10-27)))
       (letrec ((join (lambda (x)
                    (if (null? x)
                        '()
                        (let ((row ((binary-search < car) table (cadar x))))
                            (cons (list (caar x) (cadr row)) (join (cdr x))))))))
            (let ((ls (join table10-17)))
                ((nat-mergesort (lambda (a b)
                                    (string<? (car a) (car b)))) ls)))))
    

(writeln (join table10-17))

(exit)