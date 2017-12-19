(defun sledbenici (mat boja)
	(generisi-stanja '() (u-niz mat) 0 0 boja))


 (defun generisi-stanja (pred sled ind kol boja)
	(cond 
		((equal ind *size*) '())
		((equal (car sled) 2)
			(append (gen-stanje2 pred sled ind kol boja)
				(generisi-stanja (append pred (list (car sled))) ;m
				 (cdr sled) (1+ ind) (cnt kol) boja)))
		(t (generisi-stanja (append pred (list (car sled)))
			(cdr sled) (1+ ind) (cnt kol)  boja))))

		;kad je nadjeno prazno mesto 2
(defun gen-stanje2 (pred sled ind kol  boja)
	; (cond 
	; 	((equal ind ) '())
		(append (gen-hor-ver (cdr sled) ind kol '(0 2)  boja)
			(gen-hor-ver  (reverse pred) ind kol '(1 3) boja))) ;append sa cons od sled na reverse pred


;lvl1
(defun gen-hor-ver (sled ind kol h_v boja)
	(append (hor-poc (horizontalni sled kol (car h_v)) ind kol (car h_v) boja) 
		(hor-poc (vertikalni sled) ind kol (cadr h_v) boja)))



 (defun stampa(sled ind kol h_v boja)
 	(format t "indeks (~{~A~^, ~})~%" (u-index ind kol))
 		(format t "sledeci (~{~A~^, ~})~%"sled)
	(format t "index ~A~%" ind)
	(format t "kolona ~A~%" kol)
	(format t "kuda ~A~%" h_v)
	(format t "boja ~A~%" boja))

;lvl2
(defun hor-poc (sled ind kol h_v boja)
	;(if (equal ind 61) (stampa sled ind kol h_v boja))


	(cond
		((equal ind *size*) '())
		((equal (car sled) (neboja boja)) 
			(gen-hor  (cdr sled) (h-v h_v ind)  (u-index ind kol)  (norm (h-v h_v kol)) h_v boja)) 
			;ako je sledeca suprotna dodaje je u baffer i preskace
		(t (gen-hor sled ind (u-index ind kol) kol h_v boja))) )

;lvl3
(defun gen-hor (sled ind poc kol h_v boja)
	(cond 
		((null sled) '())
		;((or (> kol 8) (< kol 0)) nil)
		((equal (car sled)(neboja boja)) '())
		((equal (car sled) boja) 
			 	(list(list poc (u-index (h-v h_v ind) (norm (h-v h_v kol))))))
		(t (gen-hor (cdr sled) (h-v h_v ind) poc (norm (h-v h_v kol)) h_v boja))))



(defun norm (y)
	(mod y *n*))

(defun cnt(kol)
	(let ((cnt (1+ kol)))
	(if (equal cnt *n*) 0 cnt)))

(defun u-niz (mat)
	(if (null mat) nil
		(append (car mat) (u-niz (cdr mat)))))


(defun matrica (obr ost i n)
	(cond
		((null ost) (list obr))
		((equal i n) (append (list obr) (matrica '() ost 0 n)))
		(t (matrica (append obr (list (car ost))) (cdr ost) (1+ i) n))))

(defun u-index(ind kol )
	(cons (div (- ind kol) *n*) (list kol)))

 (defun div (x y) 
 	(/ (- x (mod x y)) y))

 (defun vertikalni (sled)
	(let ((on_tab (nth (1- *n*) sled)))
		(cond
			((null on_tab) '())
			(t (cons on_tab (vertikalni (nthcdr *n* sled)))))))

 (defun horizontalni (sled y h_v)
	(let ((on_tab (h-v h_v y)))
		(cond
			((null sled) '())
			((or (> on_tab 8) (< on_tab 0)) nil)
			(t (cons (car sled) (horizontalni (cdr sled) on_tab h_v))))))

 (defun h-v (h_v e)
 	(cond
 		((equal h_v 0) (1+ e))
 		((equal h_v 1) (1- e))
 		((equal h_v 2) (+ *n* e))
 		((equal h_v 3) (- e *n*))))

