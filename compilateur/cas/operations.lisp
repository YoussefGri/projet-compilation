;; Compilation d'opérations arithmétiques

(defun compilation-operation (exp env fenv nomf)
  (let ((op (car exp)) ;; on recupère l'opérateur
	(arg (cdr exp))) ;; on recupère les arguments
    (if (null (cddr arg)) ;; si deux arguments seulement 
	(append (compilation (car arg) env fenv nomf) ;; compilation au cours de laquelle R0 contiendra la valeur du premier argument
			'((PUSH :R0)) ;; on empile le contenu de R0
			(compilation (cadr arg) env fenv nomf) ;; compilation du second argument
			'((PUSH :R0)) ;; on empile
			'((POP :R1))  ;; récupère le sommet de la pile (contenant le second argument) dans R1
			'((POP :R0))  ;; récupère le sommet de la pile (contenant le premier argument) dans R0
		
		(case op ;; on effectue l'opération en fonction de l'opérateur
		  ('+ '((ADD :R1 :R0)))
		  ('- '((SUB :R1 :R0)))
		  ('* '((MULT :R1 :R0)))
		  ('/ '((DIV :R1 :R0)))))      
      (append (compilation  `(,op ,(list op (car arg) (cadr arg)) ,@(cddr arg)) env fenv nomf)) 
	  ;; si plus de deux arguments on compile récursivement 
	  ;; exemple (+ 1 2 3 4) devient (+ (+ 1 2) 3 4) puis (+ (+ (+ 1 2) 3) 4) et enfin (+ (+ (+ (+ 1 2) 3) 4))
	  ;; la virgule "," permet de remplacer un paramètre par sa valeur dans une liste sans que la liste ne soit évaluée
	  ;; @ permet d'insèrer les valeurs de la liste dans la liste principale l'un après l'autre au lieu de les insérer en tant que liste
      )
    )
  )