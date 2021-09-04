(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define (merge rel lst1 lst2)
    (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        ((rel (car lst1) (car lst2)) (cons (car lst1) (merge rel (cdr lst1) lst2)))
        (else (cons (car lst2) (merge rel lst1 (cdr lst2))))))

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

(define tablelO-17
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
                            (string<? (car x) (car y)))) tablelO-17))
(newline)

(writeln ((nat-mergesort (lambda (x y)
                            (< (cadr x) (cadr y)))) tablelO-17))
(newline)

(writeln ((nat-mergesort (lambda (x y)
                            (> (cadr (cddddr x))  (cadr (cddddr y))))) tablelO-17))
(newline)

(writeln ((nat-mergesort (lambda (x y)
                            (< (cadddr x)  (cadddr y)))) tablelO-17))

(display '--------10.5)
(newline)

(define (list-linear-search ls n)
    (letrec ((helper (lambda (ls n x)
                        (cond ((null? ls) #f)
                              ((= (car ls) n) x)
                              (else (helper (cdr ls) n (add1 x)))))))
        (helper ls n 0)))

(writeln (list-linear-search '(1 2 3 4 5 6 7 8 9) 8))
(exit)