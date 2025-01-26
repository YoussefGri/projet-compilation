;; Compilation de structures de condition.

(defun compilation-if (exp env fenv nomf)
  (let ((sinon (gensym "sinon"))
	(finSi (gensym "finSi")))
    (append (compilation (car exp) env fenv nomf)  
	    '((CMP :R0 (:DIESE nil))) ;; condition fausse, faut sauter au sinon
	    `((JEQ (@ ,sinon)))
	    (compilation (cadr exp) env fenv nomf) ;; compilation du then
	    `((JMP (@ ,finSi))) ;; on a fini de compiler le then on insère l'etiquette finSi
	    `((@ ,sinon)) ;; on insère l'étiquette sinon pour le else
	    (compilation (caddr exp) env fenv nomf) ;; on compile le else
	    `((@ ,finSi))) ;; on termine le if
    )
  )

(defun compilation-conditionnel (exp etiqfin env fenv nomf)
  (if (null exp)
      (append '((MOVE (:DIESE NIL) :R0)) `((@ , etiqfin)));; si on a fini de compiler les conditions, on met NIL dans R0 et on saute à la fin.      
    (let ((etiqcond (gensym "etiqcond")))
      (append (compilation (caar exp) env fenv nomf) ;; on compile le test (arg est une liste de (test action) (test action) ...) on prend le test de la première paire            
	      '((CMP :R0 (:DIESE NIL))) ;; on compare le résultat du test avec NIL
	      `((JEQ (@ ,etiqcond))) ;; on insère un saut vers le test suivant au cas ou le test serait faux 
	      (compilation (cadar exp) env fenv nomf) ;; on compile l'action associée au premier test si le test est vrai (le saut n'est pas executé quand le test est vrai)             
	      `((JMP (@ ,etiqfin))) ;; pour terminer, ça sert à rien de continuer
	      `((@ ,etiqcond)) ;; on insère une étiquette marquant le début du test suivant                           
	      (compilation-conditionnel (cdr exp) etiqfin env fenv nomf)) ;; on poursuit la compilation récursivement pour compiler le reste des tests
      )
    )
  )
  