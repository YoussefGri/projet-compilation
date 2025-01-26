;; Compilation de littéraux.

(defun compilation-constante (exp)
  `((MOVE (:DIESE ,exp) :R0)) ;; On insère un ((MOVE (:DIESE 40) :R0)) equiv de (MOVE #40, R0) où #40 est la valeur immédiate 40.
  )

(defun compile-variable-global (exp)
  `((MOVE (:* :@ ,exp) :R0)) ;; * ou @ sont des symboles faisant des accès memoire on fait donc un ((MOVE (:* :@ 40) :R0))  ;; Interprété comme: MOVE [40], R0
  )

(defun compilation-litt (exp env fenv nomf)
;; assoc cherche la clé exp dans la liste d'associations env. Si exp est trouvé dans l'environnement, var prendra la paire correspondante (<exp> . <valeur>). sinon nil
  (let ((var (assoc exp env))) 
    (cond
     ((not (null var))
      (if (eql (cadr var) 'loc) ;; si le deuxième élément de la paire est 'loc c'est donc une variable locale
	  `((MOVE ,(cdr var) :R0)) ;; si c'est ok on genere un instruction qui met la valeur de la variable dans R0
	(if (numberp (cadr var)) ;; sinon si le deuxième élément de la paire est un nombre c'est donc une constante 
	    (compilation-constante (cdr var)))
	)
      )
     ((and (symbolp exp) (not (null exp))) (compile-variable-global exp)) ;; si c'est un symbole on le traite comme une variable globale
     (t (compilation-constante exp)) ;; Si exp ne correspond ni à une variable locale ni à une variable globale, elle est traitée comme une constante.
     )
    )
  )