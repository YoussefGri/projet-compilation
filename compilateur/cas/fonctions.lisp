;; Compilation de fonctions.

(defun compilation-defun (exp env fenv nomf);; Récupère l'environnement des fonctions pour connaître le niveau d'encapsulation de `nomf`.
  (let ((nivem (assoc nomf fenv)))
    (append '((FENTRY)) ;; Marque le début de la définition de la fonction (balise d'entrée).
	    `((@ ,(car exp))) ;; @ nomfonction , Étiquette pour le nom de la fonction, permet à l'assembleur de reconnaître son point de départ.
	    (compilation-progn (cddr exp) ;; on compile sequentiellment le corps de la fonction ,  Le corps est constitué des expressions à partir du troisième élément de `exp`.
			       (param-env (cadr exp) env 1 (if nivem (+ 1 (cadr nivem)) 0)) ;; Ajoute les paramètres de la fonction dans l'environnement avec :- Leur position (déterminée par `dep`) - Leur niveau (déterminé par `nivem`).
			       (fun-env (list exp) fenv (if nivem (+ 1 (cadr nivem)) 0))  ;; Met à jour l'environnement des fonctions.
			       (car exp))  ;; Le nom de la fonction.
	    '((RTN))   ;; Instruction indiquant la fin de la fonction et le retour au point d'appel.
	    '((FEXIT))) ;; Balise marquant la sortie définitive de la fonction.
    )
  )

(defun param-env (exp env dep nivem)   
  (if (atom exp) 
      env   ;; Si plus de paramètres à traiter, retourne l'environnement.
    (param-env (cdr exp) (cons (cons (car exp) `(LOC ,(- 0 dep) ,nivem)) env) (+ 1 dep) nivem) ;;  Ajoute un paramètre avec sa position et son niveau à l'environnement.
    )  ;; Ajoute `(param (LOC décalage niveau))`. (LOC position des parametres dans la pile ) et + 1 dep  Incrémente la position du paramètre. , (nivem) Conserve le niveau d'encapsulation.
  )

(defun fun-env (exp fenv nivem)
  (if (atom exp) 
      fenv ;; Si plus de fonctions à traiter, retourne l'environnement des fonctions.
    (fun-env (cdr exp) (cons `(,(caar exp) ,nivem) fenv) nivem)     ;; Ajoute une fonction avec son niveau d'encapsulation à l'environnement des fonctions. 
    ) ;; (cdr exp )  Passe à la fonction suivante. / (cons ... )  Ajoute `(nom-fonction niveau)` à `fenv`.
  )

(defun compilation-appel (exp env fenv nomf) 
  (let ((n (length (cdr exp))) ;; Récupère le nombre d'arguments passés à la fonction.
	(nivem (assoc (car exp) fenv)));; Cherche dans l'environnement des fonctions (fenv) pour trouver le niveau d'encapsulation.
    (append (compilation-parametre (cdr exp) env fenv nomf) ;; Construit la liste d'instructions pour la compilation de l'appel de fonction. 1. Compile et empile les arguments de la fonction.
      ;; Chaque paramètre est transformé en une série d'instructions.
	  ;; 2. Empile le nombre total d'arguments sur la pile.(push)
      ;; La machine virtuelle utilisera cette information pour gérer l'appel.
	    `((PUSH (:DIESE ,n)))
	  ;; 3. Sauvegarde l'ancien pointeur de base (FP) dans :R1.
      ;; Cela garantit que l'état de la pile pourra être restauré après l'exécution de la fonction.
	    `((MOVE :FP :R1))
	  ;; 4. Met à jour le pointeur de base (FP) pour qu'il pointe sur le sommet de la pile (SP).
      ;; Cela initialise un nouveau cadre de pile pour l'appel de fonction.
	    `((MOVE :SP :FP))
	  ;; 5. Copie l'adresse actuelle du sommet de la pile dans :R2 pour travailler avec.
      ;; :R2 sert de registre temporaire pour gérer les paramètres.
	    `((MOVE :SP :R2))
	  ;; 6. Réserve de l'espace pour les arguments.
      ;; La pile est ajustée pour inclure les emplacements nécessaires pour `n` arguments.
	    `((SUB  (:DIESE ,n) :R2))
	  ;; 7. Réserve un emplacement supplémentaire pour d'autres besoins de la fonction.
      ;; Cela inclut notamment la gestion du contexte local.
	    `((SUB  (:DIESE 1) :R2))
      ;; 8. Empile l'adresse calculée des arguments sur la pile.
      ;; Cela permet à la machine virtuelle de retrouver facilement les paramètres pendant l'exécution.
	    `((PUSH :R2)) 
	  ;; 9. Empile l'ancien pointeur de base (FP).
      ;; Permet de restaurer l'état une fois que la fonction appelée est terminée.
	    `((PUSH :R1))
	  ;; 10. Empile le niveau d'encapsulation pour la fonction appelée.
      ;; Si la fonction appelée est connue (dans `fenv`), on utilise son niveau d'encapsulation.
      ;; Sinon, on empile une valeur par défaut (0).
			(if nivem 
			`((PUSH (:DIESE ,(cadr nivem))))  ;; Si le niveau d'encapsulation est défini.
			`((PUSH (:DIESE ,0))))
	  ;; 11. Effectue l'appel de la fonction via une instruction `JSR` (Jump to Subroutine).
      ;; La fonction est identifiée par son nom (car exp), qui est résolu dynamiquement.
	    `((JSR (@ ,(car exp)))))
    )
  )

(defun compilation-parametre (exp env fenv nomf)
  (if (atom exp) 
      ()      ;; Si la liste des arguments est vide, ne rien faire.
	 ;; Sinon, compile le premier argument (car exp), empile son résultat,
    ;; puis continue avec le reste des arguments (cdr exp).
    (append (compilation (car exp) env fenv nomf)
	    `((PUSH :R0))  ;; Empile le résultat dans R0.
	    (compilation-parametre (cdr exp) env fenv nomf))
    )
  )

(defun compilation-lambda (exp env fenv nomf)
;; Génère un symbole unique pour représenter cette lambda.
  (let ((lambdaexpr (gensym "lambdaexpr"))
        ;; Nombre de paramètres dans la définition de la fonction.
	(n (length (cdr exp)))
        ;; Récupère le niveau d'encapsulation actuel dans l'environnement des fonctions.
	(nivem (assoc nomf fenv)))
;; On construit la liste d'instructions pour la compilation.
    (append 
	   ;; 1. Empile les paramètres de la lambda un par un.
      ;; Cela transforme chaque paramètre en instructions d'empilement.
(compilation-parametre (cdr exp) env fenv nomf)
;; 2. Empile le nombre total de paramètres.
      ;; Cela informe la VM du nombre de paramètres attendus par la fonction.
	    `((PUSH (:DIESE ,n)))
 ;; 3. Sauvegarde l'ancien pointeur de base (frame pointer - FP) dans :R1.
      ;; Permet de restaurer l'état après l'exécution de la fonction.
	    `((MOVE :FP :R1))
;; 4. Met à jour le pointeur de base (FP) pour qu'il pointe sur le sommet de la pile (SP).
      ;; Cela initialise le cadre de pile de la fonction.
	    `((MOVE :SP :FP))
;; 5. Copie l'adresse actuelle du sommet de pile dans :R2 pour travailler avec.
      ;; :R2 est utilisé comme pointeur temporaire pour gérer les paramètres.
	    `((MOVE :SP :R2))
;; 6. Réserve de l'espace pour les paramètres et le contexte.
      ;; `n` correspond au nombre de paramètres, et on ajuste la pile pour les inclure.
	    `((SUB  (:DIESE ,n) :R2))	
	    `((SUB  (:DIESE 1) :R2))
;; 7. Empile l'adresse calculée pour les paramètres sur la pile.
      ;; Permet de gérer dynamiquement les valeurs dans le cadre de la fonction.
	    `((PUSH :R2))
;; 8. Empile l'ancien pointeur de base (frame pointer - FP).
      ;; Permet de restaurer l'état après la fin de la fonction.
	    `((PUSH :R1))
 ;; 9. Empile le niveau d'encapsulation.
      ;; Cela dépend de l'existence de `nivem` dans l'environnement.
	    (if nivem  
		`((PUSH (:DIESE ,(+ 1 (cadr nivem)))))  ;; Si on a une profondeur définie.
		`((PUSH (:DIESE ,0))))					 ;; Sinon, utilise 0 comme valeur par défaut.
;; 10. Réserve une constante zéro pour le contexte d'exécution.
      ;; Cela prépare la pile pour des extensions futures ou des variables globales.
	    `((PUSH (:DIESE 0)))
;; 11. Compile le corps de la lambda.
      ;; Cela génère les instructions pour exécuter le corps avec les paramètres et l'environnement.
	    (compilation (caddar exp)
;; Mise à jour de l'environnement pour inclure les paramètres de la fonction.
			 (param-env (cadar exp) env 1 (if nivem   (+ 1 (cadr nivem)) 0)) 
;; Mise à jour de l'environnement des fonctions pour inclure cette lambda.
			 (fun-env  (list (cons lambdaexpr (cdar exp))) fenv (if nivem (+ 1 (cadr nivem)) 0))
			 lambdaexpr)
;; 12. Restaure l'ancien sommet de la pile (SP) à partir du premier élément du cadre.
      ;; Cela nettoie la pile après l'exécution de la fonction.
	    `((MOVE ( 1 :FP) :SP))
;; 13. Restaure l'ancien pointeur de base (FP) à partir du second élément du cadre.
      ;; Cela remet l'état à celui d'avant l'appel de la fonction.
	    `((MOVE ( 2 :FP) :FP)))
    )
  )