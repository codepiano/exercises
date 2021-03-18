; 1.4
(display '--------1.4)
(newline)

(display (- (* 4 7) (+ 13 5)))
(newline)
(display (* 3 (+ 4 (- -5 -3))))
(newline)
(display (/ 2.5 (* 5 (/ 1 10))))
(newline)
(display (* 5 (+ (* 537 (+ 98.3 (- 375 (* 2.5 153)))) 255)))
(newline)
(newline)

; 1.5
(display '--------1.5)
(newline)

(display (quote "a+((b+r)-a)"))
(newline)
(display (quote "(a*b)+(r*b)"))
(newline)
(display (quote "(a-b)/(a-r)"))
(newline)
(newline)


; 1.6
(display '--------1.6)
(newline)

(display (cons 'one (cons 'two (cons 'three (cons 'four '())))))
(newline)
(display (cons 'one (cons (cons'two (cons 'three (cons 'four '()))) '())))
(newline)
(display (cons 'one  (cons (cons 'two (cons 'three '())) (cons 'four '()))))
(newline)
(display (cons (cons 'one (cons 'two '())) (cons (cons 'three (cons 'four '())) '())))
(newline)
(display (cons (cons (cons 'one '()) '()) '()))
(newline)
(newline)

; 1.13
(display '--------1.13)
(newline)
(display (car (cdr (cdr '(b c a d)))))
(newline)
(display (car (cdr (car '((b a) (c d))))))
(newline)
(display (car (car (cdr '((d c) (a) b)))))
(newline)
(display (car (car (car '(((a)))))))
(newline)

(exit)