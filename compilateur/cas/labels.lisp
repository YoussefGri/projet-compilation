;; Compilation des labels.

(defun compilation-labels (exp env fenv nomf)
  (let ((nivem (assoc nomf fenv)) ;; Récupère le niveau d'encapsulation de la fonction actuelle dans l'environnement des fonctions.
(corps (gensym "CORPS")))         ;; Génère un label unique pour le corps principal du `labels`.
    (append  ;; Insère un saut vers le corps principal pour éviter l'exécution immédiate des fonctions locales.
    `((JMP (@ ,corps)))
;; Compile les fonctions locales définies dans `labels`.
	    (compilation-faux 
      (car exp)  ;; Liste des définitions de fonctions locales.
      env 
      (fun-env (car exp) fenv (+ 1 (cadr nivem)) )  ;; Ajoute ces fonctions à l'environnement fonctionnel avec un niveau ajusté.
      nomf)
      ;; Insère un label pour commencer le corps principal du `labels`.
	    `((@ ,corps))
    ;; Compile l'expression principale dans le corps du `labels`. 
	    (compilation (cadr exp) env (fun-env (car exp) fenv (+ 1 (cadr nivem)) ) nomf))
    )
  ) 

(defun compilation-faux (exp env fenv nomf)
  (if (null exp) 
      ();; Si la liste des fonctions est vide, retourne une liste vide.
;; Sinon, compile chaque fonction locale.
    (let ((nivem (assoc (caar exp) fenv)));; Récupère le niveau d'encapsulation pour la fonction actuelle.
      (append '((FENTRY));; Marque le début d'une fonction.
	      `((@ ,(caar exp)));; Insère un label pour la fonction actuelle.
        ;; Compile le corps de la fonction avec les paramètres ajoutés à l'environnement local.
	      (compilation (caddar exp)  
        (param-env (cadar exp) env 1 (cadr nivem) ) ;; Ajoute les paramètres de la fonction à l'environnement.
         fenv (caar exp))
	      '((RTN)) ;; Insère une instruction de retour pour terminer la fonction.
	      '((FEXIT));; Marque la fin de la fonction.
	      (compilation-faux (cdr exp) env fenv nomf))
      )
    )
  )