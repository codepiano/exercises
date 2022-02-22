(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(display '--------4.1)
(newline)

(define apply-in-underlying-scheme apply)

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp) (make-procedure (lambda-parameters exp)
                                      (lambda-body exp) env))
          ((begin? exp)
           (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond->if exp) env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
           ((compound-procedure? procedure)
                (eval-sequence
                    (procedure-body procedure)
                    (extend-environment
                        (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))
           (else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env) (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)

(define (list-of-values-lr exps env)
    (if (no-operands? exps)
        '()
        (let ((x (eval (first-operand exps) env)))
            (cons x
                (list-of-values (rest-operands exps) env)))))

(define (list-of-values-rl exps env)
    (if (no-operands? exps)
        '()
        (let ((x (list-of-values (rest-operands exps) env)))
            (cons (eval (first-operand exps) env) x))))

(display '--------4.2)
(newline)

(define (self-evaluating? exp)
    (cond ((number? exp) #t)
          ((string? exp) #t)
          (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp) (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

(define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (application?-rp exp) (tagged-list? exp 'call)) 

(define (operator-rp exp) (cadr exp)) 

(define (operands-rp exp) (cddr exp)) 

(display '--------4.4)
(newline)

(define (and? exp) (tagged-list? exp 'and)) 
(define (and-predicates exp) (cdr exp)) 
(define (first-predicate seq) (car seq)) 
(define (rest-predicates seq) (cdr seq)) 
(define (no-predicate? seq) (null? seq)) 

(define (eval-and-predicates exps env) 
    (cond ((no-predicates? exps) #t) 
          ((true? (eval (first-predicate exps) env)) (eval-and-predicate (rest-predicates exps) env))
          (else #f))) 
 
(define (or? exp) (tagged-list? exp 'or)) 
(define (or-predicates exp) (cdr exp)) 
(define (eval-or-predicates exps env) 
    (cond ((no-predicates? exps) false) 
          ((true? (eval (first-predicate exps) env)) #t) 
          (else (eval-or-predicate (rest-predicates exps) env)))) 
 
 ; nested if
(define (and->if exp) 
    (expand-and-predicates (and-predicates exp))) 

(define (expand-and-predicates predicates) 
    (if (no-predicates? predicates) 
        #t
        (make-if (first-predicate predicates) 
                 (expand-predicates (rest-predicates predicates)) 
                 #f))) 
 
(define (or->if exp) 
    (expand-or-predicates (or-predicates exp))) 

(define (expand-or-predicates predicates) 
    (if (no-predicate? predicates) 
        #f
        (make-if (first-predicate predicates) 
                #t
                (expand-predicates (rest-predicates predicates)))))

(display '--------4.5)
(newline)

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals))))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                  ((eq? var (car vars)) (car vals))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                  ((eq? var (car vars)) (set-car! vals val))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable: SET!" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
    (define (scan vars vals)
        (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?) 
          (list 'cadr cadr) 
          (list 'assoc assoc)))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define (setup-environment)
    (let ((initial-env (extend-environment (primitive-procedure-names)
                                           (primitive-procedure-objects)
                                           the-empty-environment)))
          (define-variable! 'true true initial-env)
          (define-variable! 'false false initial-env)
        initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
        (let ((input (read)))
            (let ((output (eval input the-global-environment)))
                (announce-output output-prompt)
                (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))

(define (announce-output string) (newline) (display string) (newline))

(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
        (display object)))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-recipient? clause) (eq? '=> (cadr clause)))

(define (cond-recipient-action clause) (caddr clause))

(define (cond-actions clause) 
    (if (cond-recipient? clause)
        (cond-recipient-action clause)
        (cdr clause)))

(define (cond->if exp) 
    (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last: COND->IF" clauses))
                (let ((first-p (cond-predicate first)))
                        (let ((evaluated (eval (cond-predicate first) the-global-environment)))
                            (if (cond-recipient? first)
                                (make-if first-p
                                    (sequence->exp (list (list (lookup-variable-value (cond-actions first) the-global-environment)
                                                        evaluated)))
                                    (expand-clauses rest))
                                (make-if first-p
                                    (sequence->exp (cond-actions first))
                                    (expand-clauses rest))))
                        )))))

(writeln (eval '(assoc 'b '((a 1) (b 2))) the-global-environment))
(writeln (expand-clauses '(((assoc 'b '((a 1) (b 2))) => cadr) (else false))))

(display '--------4.6)
(newline)

(exit)