;; Compilation de structures itératives.

(defun compilation-progn (exp env fenv nomf)
  (if (null exp) 
      () ;; Si aucune expression, renvoie une liste vide (rien à compiler) / Sinon, compile la première expression puis les suivantes récursivement.
    (append (compilation (car exp) env fenv nomf) (compilation-progn (cdr exp) env fenv nomf)) ;; on compile chaque expression du progn sequentiellement (1ere ensuite ..)
    )
  )

(defun compilation-boucle-general (exp env fenv nomf) ;; Vérifie le type de boucle (while ou until) et appelle la fonction de compilation correspondante.
  (case (car exp)
   ('while (compilation-while (cdr exp) env fenv nomf)) ;; Si c'est une boucle while, compiler avec compilation-while.
    ('until (compilation-until (cdr exp) env fenv nomf)) ;; Si c'est une boucle until, compiler avec compilation-until.
    )
  )


(defun compilation-while (exp env fenv nomf)  ;; Crée des étiquettes uniques pour la boucle et la fin de la boucle.
  (let ((fin (gensym "finwhile"))    ;; Étiquette pour marquer la fin de la boucle. gensym Génère un nom de label unique, ici pour le corps principal. 
	(boucle (gensym "while")))      ;; Étiquette pour marquer le début de la boucle.
    (if (eql (cadr exp) 'do)          ;; Vérifie que la syntaxe est correcte (le mot-clé `do` doit être présent).
	(append `((@ ,boucle))            ;; Marque le début de la boucle avec une étiquette unique.
		(compilation (car exp) env fenv nomf) ;; Compile la condition de la boucle.
		`((CMP :R0 (:DIESE nil)))            ;; Compare la valeur dans R0 avec NIL (condition de sortie).
		`((JEQ (@ ,fin)))                   ;; Si la condition est fausse (R0 == NIL), saute à l'étiquette de fin.
		(compilation (caddr exp) env fenv nomf)   ;; Compile le corps de la boucle.
		`((JMP (@ ,boucle)))                ;; Revient au début de la boucle pour réévaluer la condition.
		`((@, fin)))                      ;; Marque la fin de la boucle avec une étiquette unique.
      (error "Syntaxe incorrecte : ~s" exp)     ;; Si la syntaxe est incorrecte, déclenche une erreur.
      )
    )
  )

(defun compilation-until (exp env fenv nomf)  ;; Crée des étiquettes uniques pour la boucle et la fin de la boucle.
  (let ((finuntil (gensym "FINUNTIL"))        ;; Étiquette pour marquer la fin de la boucle until.
	(boucle (gensym "UNTIL")))                  ;; Étiquette pour marquer le début de la boucle.
    (append `((@ ,boucle))                    ;; Marque le début de la boucle avec une étiquette unique.
	    (compilation (car exp) env fenv nomf)   ;; Compile la condition de la boucle.
	    '((CMP :R0 (:DIESE T)))               ;; Compare la valeur dans R0 avec T (condition de sortie pour until).
	    `((JEQ (@ ,finuntil)))                ;; Si la condition est vraie (R0 == T), saute à l'étiquette de fin.
	    (compilation (caddr exp) env fenv nomf)      ;; Compile le corps de la boucle.
	    `((JMP (@ ,boucle)))                ;; Revient au début de la boucle pour réévaluer la condition.
	    `((@ ,fin)))                        ;; Marque la fin de la boucle avec une étiquette unique.
    )
  )