;; Compilation des déclarations de variables.

(defun compilation-setf (exp env fenv nomf)  
  (if (null exp) ();; Vérifie si l'expression est vide. Si ouion ne fais rien. 
    (append (compilation (cadr exp) env fenv nomf)  ;; Compile la partie de l'expression correspondant à la valeur à attribuer (cadr exp).
	    (let ((var (assoc (car exp) env)))          ;; Vérifie si la variable (car exp) est déjà définie dans l'environnement, on stocke dans var (x . (LOC 1 0)) par ex.
	      (if var                                     ;; Si la variable existe dans l'environnement :
		  (if (eql (cadr exp) 'loc) '((MOVE :R0 (cdr var))))    ;; Si c'est une variable locale, génère une instruction pour déplacer la valeur dans son emplacement mémoire.
		`((VARG ,(car exp)))                                  ;; Si la variable n'est pas trouvée, génère une instruction pour déclarer une nouvelle variable qui sera globale. Déclare une nouvelle variable avec VARG.
		)
	      )
	    `((MOVE :R0 ,(cadar (compilation (car exp) env fenv nomf))))  ;; Génère une instruction pour déplacer le contenu de R0 dans la destination appropriée.
	    (compilation-setf (cddr exp) env fenv nomf))   ;; première réaffectation ok, Appelle récursivement compilation-setf sur le reste.
    )
  )
      
(defun compilation-let (exp env fenv nomf)
  (let ((nivem (assoc nomf fenv)))  ;; Récupère le niveau d'encapsulation pour la fonction actuelle.
  ;; Concatène les étapes de la compilation de l'expression `let`.

    (append 
;; Étape 1 : Compiler les affectations locales et empiler les résultats. 
    (compilation-local (car exp) env fenv nomf)      
;; Étape 2 : Compiler l'expression principale en utilisant l'environnement local modifié. 
     (compilation 
    (cadr exp)  ;; L'expression principale du `let`.
    (local-env (car exp) env 1 (cadr nivem))   ;; Ajoute les variables locales à l'environnement.
    fenv nomf)
      ;; Étape 3 : Dépiler les variables locales après l'évaluation.

	    (depile-local (car exp) env fenv nomf))
    )
  )

(defun compilation-local (exp env fenv nomf)
  (if (null exp) 
      () ;; Si aucune variable à traiter, retourne une liste vide.
    ;; Sinon, compile l'initialisation de la variable locale.
    (append (compilation (cadar exp) env fenv nomf) ;; Compile l'expression associée à la variable locale.
	    '((PUSH :R0)) ;; Empile la valeur calculée (dans R0) sur la pile.
	    (compilation-local (cdr exp) env fenv nomf ))  ;; Passe à la prochaine variable locale.
    )
  )

(defun depile-local (exp env fenv nomf)
  (if (null exp) 
      () ;; Si aucune variable locale à dépiler, retourne une liste vide.
    ;; Sinon, dépile la variable locale actuelle.
    (append '((POP :R1)) ;; Dépile une valeur de la pile dans le registre `R1`.
    (depile-local (cdr exp) env fenv nomf));; Passe à la prochaine variable locale.
    )
  )

(defun local-env (exp env dep nivem)  
  (if (atom exp) 
      env
    (local-env (cdr exp) (cons (cons (caar exp) `(LOC ,dep ,nivem)) env) (+ 1 dep) nivem)
    )
  )