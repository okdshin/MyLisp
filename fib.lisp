((lambda(x) 
   (defun fib (n) (if (eq n 1) 1 (if (eq n 0) 1 (+ (fib(- n 1)) (fib(- n 2)))))) 
   (defun p1 (x) (+ x 1))
   (fib x)) 28)

