;(defstruct stanje poz boja) 

;glavna f-ja za zavrsetak igre

(defun proveri-kraj (boja poz n mat)
	(if (or 
		(<= (izbroji (neboja boja) mat) 4)
		(vece-od 5 (izdvoji_vert boja poz mat))
		(vece-od 5 (izdvoji_dijag boja poz 0 n mat))
		(vece-od 5 (izdvoji_dijag boja poz 1 n mat))) t nil))

;broji figure za boju (crna bela)
(defun izbroji (boja mat)
	(cond 
		((null mat) 0)
		((listp (car mat)) (+ (izbroji boja (car mat)) (izbroji boja (cdr mat))))
		(t (if (equal boja (car mat))
			(1+ (izbroji boja (cdr mat)))
			(izbroji boja (cdr mat))))))

;vraca niz uzastopnih ponavjanja na dijagonali za element tj. boju (ako postoji neki >5 u nizu kraj igre)
(defun izdvoji_dijag (boja poz gl n mat)
	(let ((poz-oks (poc_dijag (pomeri_index_dijag gl (1- n) poz)))
		(poz-iks (poc_dijag (pomeri_index_dijag gl (1- n) (pomeri_index_iks (1- n) poz)))))
		(if (equal boja 0)
			(broj_susednih boja 0 
				(izdvoji_niz_dijag poz-oks n (izaberi_dijag gl mat)))
			(broj_susednih boja 0
				(izdvoji_niz_dijag poz-iks n (reverse (izaberi_dijag gl mat)))))))	

;isto kao prethodna samo za vertikalne ele 
(defun izdvoji_vert (boja poz mat)
	(if (equal boja 1) 
		(broj_susednih boja 0 (cddr (izdvoji_niz poz mat)))
		(broj_susednih boja 0 (butlast (butlast (izdvoji_niz poz mat))))))

;;;;Pomocne

;pomocna f-ja koja broji susedne u nizu
(defun broj_susednih (boja brpon niz) 
	(cond 
		((null  niz) (list brpon))
		((and (equal (car niz) boja) (equal (car niz) (cadr niz))) 
			(broj_susednih boja (1+ brpon) (cdr niz)))
		(t (cons  (if (= boja (car niz)) (1+ brpon) brpon) 
			(broj_susednih boja 0 (cdr niz))))))

;pomocna f-ja za izdvajanje vertikalnih ele
(defun izdvoji_niz (poz mat)
	(if (null mat) nil
		(cons (nth (cadr poz) (car mat)) (izdvoji_niz poz (cdr mat)))))


;;;pomocne f-je za izdvajanje dijagonalnih elemenata

;za suprotnu dijagonalu
(defun izaberi_dijag (gl mat)
	(if (= gl 1) mat (mapcar 'reverse mat)))

;indeksi za suprotnu dijag posto se inverzuje matrica po kolonama
(defun pomeri_index_dijag (gl n poz)
	(if (= gl 1) poz (list (car poz) (- n (cadr poz)))))

;indeksi za iks igraca posto se inverzuje matrica po vrstama
(defun pomeri_index_iks (n poz)
	(list (- n (car poz)) (cadr poz)))

;za poziciju trazi pocetni element za tu dijagonalu
(defun poc_dijag (poz)
	(let ((x (car poz)) (y (cadr poz))) 
	(cond 
		((equal x y) '(0 0))
		((> x y) (list (- x y) 0))
		(t (list 0 (- y x))))))

;vraca niz dijagonale od poc elementa
(defun izdvoji_niz_dijag (poc n mat)
	(cond 
		((= (car poc) (- n 2)) nil)
		(t (if (null (ij poc mat)) nil
			(cons (ij poc mat) (izdvoji_niz_dijag (mapcar '1+ poc) n mat))))))

;vraca ij element matrice
(defun ij (poz mat)
	(nth (cadr poz) (nth (car poz) mat))) 

;ne boja -> suprotno od boje vraca

(defun neboja (boja)
	(if (= boja 1) 0 1))

(defun vece-od (broj niz)
	(cond
		((null niz) nil)
		((>= (car niz) broj) t)
		(t (vece-od broj (cdr niz)))))