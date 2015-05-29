(begin (define isEven (lambda (x) (begin (comment (define y (+ x 10))) (if (lt? x 0) (if (eqv? (mod (/ x -1) 2) 0) \"even\" \"odd\" ) (if (eqv? (mod x 2) 0) \"even\" \"odd\" )	)))) (isEven 84))
