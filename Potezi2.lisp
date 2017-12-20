
;vraca kompletna stanja

(defun vrati-sva-stanja(mat boja)
	(let ((trenutno mat))
		(iterator trenutno (sledbenici trenutno boja) boja)))

(defun iterator(mat sledbenici boja)
	(cond 
		((null sledbenici) nil)
		(t (cons (list (car sledbenici) (sendvic-top (car sledbenici) (potez mat (car sledbenici) boja) boja))
			 (iterator mat (cdr sledbenici) boja)))))


;(defun sendvic-top(potez mat boja)

;vraca listu novi stari
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


;deo pomocni

;normalizuj kolonu za trazenje po koloni
(defun norm (y)
	(mod y *n*))

;predje kolona n-1 vrati na nula
(defun cnt(kol)
	(let ((cnt (1+ kol)))
	(if (equal cnt *n*) 0 cnt)))

;deo pretvaranje matrice

;pretvara u niz 
(defun u-niz (mat)
	(if (null mat) nil
		(append (car mat) (u-niz (cdr mat)))))

;pretvara u matricu
(defun matrica (obr ost i n)
	(cond
		((null ost) (list obr))
		((equal i n) (append (list obr) (matrica '() ost 0 n)))
		(t (matrica (append obr (list (car ost))) (cdr ost) (1+ i) n))))

;kraj deo pretvaranje matrice

;na vraca indeks
(defun u-index(ind kol )
	(cons (div (- ind kol) *n*) (list kol)))

;celobrojno deljenje
 (defun div (x y) 
 	(/ (- x (mod x y)) y))

 ;daje sledece vertikalne
 (defun vertikalni (sled)
	(let ((on_tab (nth (1- *n*) sled)))
		(cond
			((null on_tab) '())
			(t (cons on_tab (vertikalni (nthcdr *n* sled)))))))
;daje sledece horizontalne
 (defun horizontalni (sled y h_v)
	(let ((on_tab (h-v h_v y)))
		(cond
			((null sled) '())
			((or (> on_tab 8) (< on_tab 0)) nil)
			(t (cons (car sled) (horizontalni (cdr sled) on_tab h_v))))))
;bira gore dole levo desno
 (defun h-v (h_v e)
 	(cond
 		((equal h_v 0) (1+ e))
 		((equal h_v 1) (1- e))
 		((equal h_v 2) (+ *n* e))
 		((equal h_v 3) (- e *n*))))

;kraj pomocnog dela

;deo za setovanje matrice


;setuje jedno polje
(defun vrsta(mat pot boja)
	(if (equal 0 (car pot)) 
		(cons (kolona (car mat) (cadr pot) boja) (cdr mat))
		(cons (car mat) (vrsta (cdr mat) (cons (1- (car pot)) (cdr pot)) boja))))

;za prethodnu
(defun kolona (niz kol boja)
	(if (equal kol 0)
		(cons boja (cdr niz))
		(cons (car niz) (kolona (cdr niz) (1- kol) boja))))

;i setuje i brise
(defun potez (mat pot boja)
	(vrsta (vrsta mat (car pot) boja) (cadr pot) 2))



;deo za setovanje sendvica

(defun sendvic-top(potez mat boja)  ;((n n) (s s)) potez
	(send2 (sendvic-niz (car potez) mat boja) mat))

(defun send2(niz mat)
	(cond 
		((null  niz) mat)
		(t (send2 (cdr niz) (send (car niz) mat)))))

(defun send(pot mat)
	(vrsta mat pot 2))
