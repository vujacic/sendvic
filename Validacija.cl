;main validaciona f-ja, boja elementa za koji ispitujemo
(defun validacijap (boja poz poz_new  mat)
	;(let ((x (car poz)) (y (cadr poz)) (i (car poz_new)) (j (cadr poz)))
	(if (and (validan-potezp poz_new)
		 	(u-opsegup poz_new mat)
		 	(boja-i-istip boja poz poz_new)
		 	(nije-dijagp poz poz_new)
		 	(let* ((indeksi (komp-za-slanje poz poz_new)) (poc (cadr indeksi)) (kr (caddr indeksi)))
				(trazi-po-redup boja poc kr (incd poc kr) (izdvoji-v-h (cadddr indeksi) (car indeksi) mat))))
		t nil)) 


;da li je assoc lista
(defun validan-potezp (poz_new)
	(if (and (listp poz_new) 
		(and (numberp (car poz_new)) (numberp (cadr poz_new)))) 
	t nil)) 

;ako uzima protivnicki ili hoce isti potez 
(defun boja-i-istip (boja poz poz_new)
	(if (or (not (equal boja *curr*)) (equal poz poz_new)) nil t))

;ako nije u opsegu
(defun u-opsegup (poz_new mat)
	(cond
		((null (ij poz_new mat)) nil)
		((equal (ij poz_new mat) 2) t)
		(t nil)))

;ako nije na dijagonali
(defun nije-dijagp (poz poz_new)
	(let ((x (car poz)) (y (cadr poz)) (i (car poz_new)) (j (cadr poz_new)))
		(if (and (not (= x i)) (not (= y j)))
			nil t)))

;trazi u nizu koji je izdvojen
(defun trazi-po-redup (boja indeks i_new incd niz)
	(do ((pomeri (funcall incd indeks) (funcall incd pomeri)))
		((= pomeri i_new) (if (= (nth i_new niz) 2) t nil))
		(if (not (= (nth pomeri niz) 2) )
			(if (and (= (abs (- i_new pomeri)) 1) 
					(if (= (nth pomeri niz) boja) nil t) 
					;(= (nth (funcall incd i_new) niz) 2)
					) 
				(return-from trazi-po-redup t) (return-from trazi-po-redup nil)))))

;x ili y komponenta koja se salje u trazi-po-redup f-ji (1- izdvojena vrsta, 0 izdvojena kolona)
(defun komp-za-slanje (poz poz_new)
	(let ((x (car poz)) (y (cadr poz)) (i (car poz_new)) (j (cadr poz_new)))
		(if (= x i) (list x y j 1) (list y x i 0))))

;vraca inc tj. decr f-ju u zavisnosti od pocetnog parametra
(defun incd (poc kr)
	(if (< poc kr) '1+ '1-))

;izdvaja niz vertikalnih ili horizontalnih elementa za poziciju
(defun izdvoji-v-h (tip poz mat)
	(if (= tip 1) (nth poz mat) (izdvoji_niz (list 0 poz) mat)))
