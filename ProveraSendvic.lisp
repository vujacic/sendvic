(defun sendvicp (spotez npotez mat boja)
	(let ((niz (sendvic-niz npotez mat boja)))
		(if (niz null) nil t)))


; ispituje elemente levo/desno/gore/dole (samo za jedan u zavisnosti od trenutnog i gde je stigo) i vraca niz za zamenu
(defun ldgd (niz tpotez npotez mat boja)
	(cond
		((equal (ij tpotez mat) nil) nil)
		((equal (ij tpotez mat) (neboja boja)) 
			(ldgd (append niz (list tpotez)) (upoldgdi-i-vrati tpotez npotez) npotez mat boja))
		((equal (ij tpotez mat) boja)  niz)))

; pomocna za ldgd vraca koji sledeci element treba ispitati
(defun upoldgdi-i-vrati (poz_t poz_new)
	(let ((x (car poz_t)) (y (cadr poz_t)) (i (car poz_new)) (j (cadr poz_new)))
		(cond
			((equal x i) (if (< y j) (cons x (list (1- y))) (cons x (list (1+ y)))))
			(t (if (< x i) (cons (1- x) (list y)) (cons (1+ x) (list y)))))))

; vraca niz sa elementima koji ce da se menjaju
(defun sendvic-niz (p_n mat boja)
	(let ((x (car p_n)) (y (cadr p_n))) 
	(append (ldgd '() (list (1+ x) y) p_n mat boja)
			(ldgd '() (list (1- x) y) p_n mat boja)
			(ldgd '() (list x (1+ y)) p_n mat boja)
			(ldgd '() (list x (1- y)) p_n mat boja))))

