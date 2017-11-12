(defun generisi(n p)
	(cond ((= p 0) '() )

		  ((> p (- n 2))
		  		(cons (loop for i from 1 to n  
		  			append (append '()  '(1))) (generisi n (- p 1))))
		  ((< p 3)
		 		(cons (loop for i from 1 to n  
		  			append (append '()  '(0))) (generisi n (- p 1))))

		  ((cons (loop for i from 1 to n  
		  			append (append '()  '(2))) (generisi n (- p 1))))
	)	

)

(defun inicijalizacija ()
               (format t "Unesi dimenzije polja:")
               (setq n (read))
               (setq pom n)
               (if (> n 7)(generisi n pom)(format t "polje mora biti 7x7 ili vece"))
               )
