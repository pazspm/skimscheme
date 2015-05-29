-- Uses if, lt? and recursion
(begin (define fatorial (lambda (x) (if (lt? x 1) 1 (* x (fatorial (- x 1)))))) (fatorial 10))
