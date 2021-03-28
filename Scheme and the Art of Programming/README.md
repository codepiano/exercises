## helper procedure

```scheme
(define error
    (lambda args
        (display "Error:")
        (for-each (lambda (value) (display " ") (display value)) args)
        (newline)))

(define writeln (lambda x (for-each display x) (newline)))
```
