(begin (define fatorial 
  (lambda (x)
    (if (lt? x 1) 1 (* x (factorial (- x 1))))
   )
  )
  (fatorial 10)
)