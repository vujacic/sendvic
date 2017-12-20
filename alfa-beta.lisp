(defun alphabeta (stanje dubina a b maximizingPlayer) 
	(if (equal maximizingPlayer 1)
		(max-value stanje dubina a b)
		(min-value stanje dubina a b)))

(defun max-value (stanje dubina a b)
	;(format t "ja sam max ")
	;(format t "~a ~%" dubina)
	(if (equal dubina 0)
		(proceni stanje)
		(do* ( (sva_stanja (vrati-sva-stanja (caddr stanje) 1) (cdr sva_stanja)) ;prom svi sledbenici
				(s (car sva_stanja) (car sva_stanja))
				(v (car sva_stanja)))						;prom trenutni
				((null sva_stanja) (cons (car a) v))			;ako je sledbenik null vrati a (stigli smo do kraja)
	;telo pozivamo min f-ju
		;(format t "stanje ~a ~%" (car sva_stanja))
				(setf v s)
				(if (vecep (setf a (maxab a (min-value (cons '() s) (1- dubina) a b))) b) (return (cons (car b) v))))))			;telo postavi a i ispitaj da li je vece od b


(defun min-value (stanje dubina a b)
	;(format t "ja sam min ")
	;(format t "dubina ~a ~%" dubina)
	;(format t "stanje ~a ~%" stanje)
	(if (equal dubina 0)
		(proceni stanje)
		(do* ( (sva_stanja (vrati-sva-stanja (caddr stanje) 0) (cdr sva_stanja)) ;prom svi sledbenici
				(s (car sva_stanja) (car sva_stanja))
				(v (car sva_stanja)))						;prom trenutni
				((null sva_stanja) (cons (car b) v))			;ako je sledbenik null vrati a (stigli smo do kraja)
				;telo pozivamo max f-ju
				;(format t "sledece stanje ~a~%" s)
				(setf v s)
				(if (manjep (setf b (minab b (max-value (cons '() s) (1- dubina) a b))) a) (return (cons (car a) v))))))		;telo postavi b i ispitaj da li je manje od a


(defun vecep (a b)
	;(format t "uso u > ~a ~a~%" a b)
	(if (>= (car a) (car b)) t nil))

(defun manjep (a b)
	;(format t "uso u < ~a ~a~%" a b)
	(if (<= (car a) (car b)) t nil))

(defun maxab (a b)
	;(format t "uso u maxab ~a ~a~%" a b)
	(if (>= (car a) (car b)) a b))

(defun minab (a b)
	;(format t "uso u minab ~a ~a~%" a b)
	(if (<= (car a) (car b)) a b))

(defun proceni (p)
	;(format t "ja procenjujem ~a ~%" p)
	(let ((stanje (caddr p)) (poz (car(cadr p))))
		(cond
			((proveri-kraj 1 poz *n* stanje) (cons 10 (cdr p)))
			((proveri-kraj 0 poz *n* stanje) (cons -10 (cdr p)))
			(t (if (> (izbroji 1 stanje)(izbroji 0 stanje)) (cons 5 (cdr p)) (cons -5 (cdr p))))))) 