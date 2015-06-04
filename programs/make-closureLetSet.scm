-- resp = #f

(begin 
	(let ((i (% 5 4)) (j (/ 16 4))) 
		(define clo 
			(make-closure 
				(lambda (x y) 
					(begin (set! x (+ i j)) (set! j (+ x y)) j)
				)
			)
		)
	) 

	(define clo1 (clo 1 2))
	(define clo2 (clo 1 2)) 
	(eqv? clo1 clo2)
)
