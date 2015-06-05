
  
(begin 
  (define part 
    (lambda (comp l) 
      (if (eqv? l '()) 
        '() 
        (if (comp (car l)) 
          (cons (car l) (part comp (cdr l))) 
          (part comp (cdr l))
        )
      )
    )
  )

  (define qsort 
    (lambda (l) 
      (if (eqv? l '()) '() (let ((p (car l))) 
        (cons
          (cons 
            (qsort (part (lambda (x) (lt? x p)) l)) 
            (part (lambda (x) (eqv? x p)) l)
          ) 
          (qsort (part (lambda (x) (lt? p x)) l))
          )
        )
      )
    )
  ) 

(qsort '(0 1 10 3 1 23 4)))"

