(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define identity (lambda (x) x))

(display '--------2.54)
(newline)

(define (my-equal? a b)
    (cond ((and (null? a) (null? b)) #t)
          ((or (null? a) (null? b) #f))
          ((and (pair? a) (not (pair? b))) #f)
          ((and (pair? b) (not (pair? a))) #f)
          ((and (not (pair? a)) (not (pair? b))) (eq? a b))
          ((and (pair? (car a)) (pair? (car b))) (and (my-equal? (car a) (car b))) (my-equal? (cdr a) (cdr b)))
          (else (if (eq? (car a) (car b))
                    (my-equal? (cdr a) (cdr b))
                    #f))))

(writeln (my-equal? 2 2))

(writeln (my-equal? 2 '(1 2 3)))

(writeln (my-equal? '(1 2 3) 2))

(writeln (my-equal? (list 1 2 (list 3 4)) (list 1 2 (list 3 4))))

(display '--------2.56)
(newline)

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))))
          ((exponentiation? expr)  
                (make-product  (make-product  
                                (exponent expr) 
                                (make-exponentiation (base expr) 
                                (make-sum (exponent expr) -1)))                                                                                                 
                    (deriv (base expr) var))) 
          (else (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num) (and (number? exp) (= exp num)))

(writeln (deriv '(* (* x y) (+ x 3)) 'x))

(define (exponentiation? x) (and (pair? x) (eq? (car x) 'expt)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

(define (make-exponentiation b expnt)
    (cond ((=number? b 1) 1)
          ((=number? expnt 0) 1)
          ((and (number? b) (number? expnt)) (expt b expnt))
          (else (list 'expt m1 m2))))

(display '--------2.59)
(newline)

(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((my-equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (union-set s1 s2)
    (cond ((null? s1) s2)
          ((null? s2) s1)
          (else (union-set (adjoin-set (car s2) s1) (cdr s2)))))

(writeln (union-set '(1 2 3) '(2 3 4)))
(writeln (union-set '(1 2 3) '(5 6 7)))

(display '--------2.60)
(newline)

(define d-adjoin-set cons)

(define d-union-set append)

(display '--------2.61)
(newline)

(define (o-adjoin-set x set)
    (cond ((null? set) (cons x set))
          ((= x (car set)) set)
          ((< x (car set)) (cons x set))
          (else (cons (car set) (o-adjoin x (cdr set))))))

(display '--------2.62)
(newline)

(define (o-union-set s1 s2)
    (cond ((null? s1) s2)
          ((null? s2) s1)
          ((<= (car s1) (car s2)) (cons (car s1) (o-union-set (cdr s1) s2)))
          (else (cons (car s2) (o-union-set s1 (cdr s2))))))

(writeln (o-union-set '(1 3 5 7) '(2 4 6 8)))

(writeln (o-union-set '(1 3 5 7) '(12 14 16 18)))

(writeln (o-union-set '(12 14 16 18) '(1 3 5 7)))

(display '--------2.65)
(newline)

(define (t-union-set s1 s2)
    (list->tree (union-set (tree->list-2 s1) (tree->list-2 s2))))

(define (t-intersection-set s1 s2)
    (intersection-set (tree->list-2 s1) (tree->list-2 s2)))

(display '--------2.66)
(newline)

(define (lookup given-key tree-set)
   (cond ((null? tree-set) #f) 
         ((= given-key (key (entry tree-set))) 
          (entry tree-set)) 
         ((< given-key (key (entry tree-set))) 
          (lookup given-key (left-branch tree-set))) 
         (else (lookup given-key (right-branch tree-set)))))

(display '--------2.67)
(newline)

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch
                    (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                        (make-leaf 'B 2)
                        (make-code-tree
                            (make-leaf 'D 1)
                            (make-leaf 'C 1)))))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(writeln (decode sample-message sample-tree))

(display '--------2.68)
(newline)

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (encode-symbol c tree)
    (cond ((null? tree) '())
          ((leaf? tree) (if (eq? c (symbol-leaf tree))
                            '()
                            (error "missing")))
          ((element-of-set? c (symbols (left-branch tree))) (cons 0 (encode-symbol c (left-branch tree))))
          ((element-of-set? c (symbols (right-branch tree))) (cons 1 (encode-symbol c (right-branch tree))))))

(writeln (encode (decode sample-message sample-tree) sample-tree))

(display '--------2.69)
(newline)

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                        (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                                   (cadr pair))
                        (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs) (successive-merge (make-leaf-set pairs)))

(define (successive-merge l)
    (define (iter x)
        (if (= (length x) 1)
            (car x)
            (let ((first (car x))
                  (second (cadr x))
                  (rest (cddr x)))
                  (iter (adjoin-set (make-code-tree first second) rest)))))
    (iter l))

(writeln sample-tree)
(writeln (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
(writeln (decode (encode '(a d a b b c a) (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))) (generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))))

(exit)