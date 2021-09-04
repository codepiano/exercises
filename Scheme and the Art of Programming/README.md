## scheme implementation

[MIT/GNU Scheme](https://www.gnu.org/software/mit-scheme/)

vscode code runner plugin config

```javascript
{
    "code-runner.runInTerminal": true,
    "code-runner.executorMapByFileExtension": {
        ".ss": "scheme"
    },
    "code-runner.executorMap": {
        "scheme": "scheme --quiet --load "
    }
}
```

## helper procedure

```scheme
(define error
    (lambda args
        (display "Error:")
        (for-each (lambda (value) (display " ") (display value)) args)
        (newline)))

(define writeln (lambda x (for-each display x) (newline)))

(define (sub1 n)
    (- n 1))

(define (add1 n)
    (+ n 1))

(define (atom? x) (not (pair? x)))

(define compose (lambda (f g)
    (lambda (x)
        (f (g x)))))

(define (merge rel l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((rel (car l1) (car l2)) (cons (car l1) (merge rel (cdr l1) l2)))
          (else (cons (car l2) (merge rel l1 (cdr l2))))))
```