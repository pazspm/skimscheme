(begin 
   (let ((x 5) (y 0)) (
  	set! x (+ x (set! y (+ x x)))

  ))  
)
-- res = 15 
