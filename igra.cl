;globalno stanje 
(defvar *matrica* '())
(defvar *n* )
(defvar *size*)
(defvar *curr* 1)
(defvar *poz*)


;init f-ja not complete, zove read eval exe f-ju koja posle povlaci poteze
(defun init (dim)
	;(format t "~CUnesi dimenzije polja:"#\linefeed)
    ;(setq n (read))
    (setq *n* dim)
    (setq *size* (* *n* *n*))
    (setq *matrica* (generisi *n* *n*))
    (setq *curr* 1)
    (format t "IKS ili OKS (1 ili 0):")
    ;(setq *human* (read))
    ;(setq *komp* (if (= *human* 1) 0 1)) ;mozda za kasnije kada imamo kompa
    (read-eval-exe))

;not complete , ne proverava postojanje sendvica
(defun napravi-potez (curr poz poz_new mat)
	(let ((x (car poz)) (y (cadr poz)) (i (car poz_new)) (j (cadr poz_new)))
		(setf (nth j (nth i mat)) curr)
		(setf (nth y (nth x mat)) 2)
		(progn 
		(postavi-sen (sendvic-niz poz_new mat curr) mat)
		(proveri-kraj curr poz_new *n* mat))))


;not complete
(defun read-eval-exe ()
	(stampaj *matrica* 35)
	(format t "~CUnesi koor:"#\linefeed)
	(setq *poz* (read))
	(if (atom *poz*) nil
	(if (validacijap (ij (car *poz*) *matrica*) (car *poz*) (cadr *poz*) *matrica*) 
		(if (napravi-potez *curr* (car *poz*) (cadr *poz*) *matrica*)
			(prog1 (format t "pobednik") ) (progn (setq *curr* (neboja *curr*)) (read-eval-exe)))
		(prog1 (format t "Pogresan potez!")
				(read-eval-exe)))))

(defun postavi (poz mat ele)
	(setf (nth (cadr poz) (nth (car poz) mat)) ele))

(defun postavi-sen (niz mat)
	(cond 
		((null (car niz)) nil)
		(t (prog1 
			(postavi (car niz) mat 2) 
			(postavi-sen (cdr niz) mat)))))