(defun fact (n) 
  (if (eq n 1) 
	1
	(* n (fact (- n 1)))))

