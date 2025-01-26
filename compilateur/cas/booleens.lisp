;; Compilation d'opérateurs de comparaison.

(defun compilation-comparaison (exp env fenv nomf)
  (let ((op (car exp))
	(fin (gensym "finTest")))
    (append (compilation (cadr exp) env fenv nomf) ;; On compile la première expression (gauche de la comparaison).
	    '((PUSH :R0)) ;; On empile le résultat de la première expression pour le comparer plus tard.
	    (compilation (caddr exp) env fenv nomf) ;; droite
	    '((PUSH :R0)) ;; empile res droite
	    '((POP :R0)) ;; on dépile et on stocke res droite dans R0
	    '((POP :R1)) ;; gauche dans R1
	    '((CMP :R1 :R0)) 
	    '((MOVE (:DIESE T) :R0)) 
	    (case op
	      ('= `((JEQ (@ ,fin))))
	      ('< `((JL (@ ,fin))))
	      ('> `((JG (@ ,fin))))
	      ('<= `((JLE (@ ,fin))))
	      ('>= `((JGE (@ ,fin))))
	      )
	    '((MOVE (:DIESE NIL) :R0))
	    `((@ ,fin)))
    )
  )

(defun compilation-and (exp etiqfin env fenv nomf)
  (if (null exp) 
      (append '((MOVE (:DIESE T) :R0)) `((@ ,etiqfin))) ;; si toutes les conditions de and ont été vérifiées sans échec.
    (append (compilation (car exp) env fenv nomf) 
	    '((CMP :R0 (:DIESE T))) ;; on insère une comparaison du résultat de l'expression courante avec T
	    `((JNE (@ ,etiqfin))) ;; si la comparaison n'est pas ok (R0 contient autre chose que T), on insère un saut vers la fin du and.
	    (compilation-and (cdr exp) etiqfin env fenv nomf))
    )
  )

(defun compilation-or (exp etiqfin env fenv nomf)
  (if (null exp) 
      (append '((MOVE (:DIESE NIL) :R0)) `((@ ,etiqfin)))
    (append (compilation (car exp) env fenv nomf)
	    '((CMP :R0 (:DIESE T)))
	    `((JEQ (@ ,etiqfin))) ;; on tombe sur un true ça ne sert à rien de faire le reste pour la vm on insere un saut vers la fin
	    (compilation-or (cdr exp) etiqfin env fenv nomf))
    )
  )