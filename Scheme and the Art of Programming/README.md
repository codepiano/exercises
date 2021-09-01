## scheme implementation

[MIT/GNU Scheme](https://www.gnu.org/software/mit-scheme/)

code runner config

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
```