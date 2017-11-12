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

(defun listt (l p)
	(setq poml '())
	(cons (code-char(+ p 65))(mapcar (lambda (y) (
		cond ((equal y 1)(append poml "x "))
			 ((equal y 0)(append poml "o "))
			 ((append poml "- ")))
	) l)
	))

(defun stampaj(l s)
	(if (< (length l) 10) (if (= s 35) (format t "~% ~a" (append '("")(loop for y from 1 to (length l) append (append '("")  (list y)))'(""))))
	(if (= s 35) (format t "~% ~a" (append '("")(loop for y from 1 to 9 append (append '("")  (list y)))(loop for y from 10 to (length l) append (append '()  (list y)))'("")))))
	(cond ((null l) '())
		  ((listp (car l))
				(format t "~% ~a" (listt (car l) (- 35 s)))
				(stampaj (cdr l) (- s 1))
		  )
	)
)

(defun inicijalizacija ()
               (format t "Unesi dimenzije polja:")
               (setq n (read))
               (setq pom n)
			   ;(if (> n 9)(stampaj2 (generisi n pom) 35))
               (if (> n 7)(stampaj (generisi n pom) 35)(format t "polje mora biti 7x7 ili vece"))
               )
