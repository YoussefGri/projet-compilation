(require "compilateur/is-inst.lisp")
(require "compilateur/all-cas.lisp")

;; Fonctions de lancement du compilateur LISP -> ASM.
;; env : l'environnement des variables locales (une liste d'associations (variable . valeur)).
;; fenv : l'environnement des fonctions (une liste des fonctions définies dans le programme).
;; nomf :  le nom de la fonction courante.
;; Les fonctions de compilation renvoient une liste de listes de symboles représentant les instructions assembleur.
;; assoc cherche la clé exp dans la liste d'associations env et renvoie (clé . valeur) si elle existe.


(defun compilation (exp &optional (env ()) (fenv ())  (nomf ()) )
  (let ((arg (if (atom exp) () (cdr exp))));; si y a un seul truc dans l'expression, on ne fait rien, sinon on met le reste sans la tete dans arg
    (cond
     ((atom exp) (compilation-litt exp env fenv nomf)) 
     ((member (car exp) '(+ - * /)) (compilation-operation exp env fenv nomf))
     ((member (car exp) '(< > = <= >= )) (compilation-comparaison exp env fenv nomf))
     ((is-instructionruction exp 'and) (compilation-and arg (gensym "finAnd") env fenv nomf)) ;; finAnd est un label unique pour la fin de l'opération AND, qui servira de point de sortie en cas de condition fausse.
     ((is-instructionruction exp 'or) (compilation-or arg (gensym "finOr") env fenv nomf))
     ((is-instructionruction exp 'if) (compilation-if arg env fenv nomf))
     ((is-instructionruction exp 'cond) (compilation-conditionnel arg (gensym "fincond") env fenv nomf))
     ((is-instructionruction exp 'progn) (compilation-progn arg env fenv nomf)) 
     ((is-instructionruction exp 'loop) (compilation-boucle-general arg env fenv nomf))
     ((is-instructionruction exp 'setf) (compilation-setf arg env fenv nomf))
     ((is-instructionruction exp 'defun) (compilation-defun arg env fenv nomf))
     ((is-instructionruction exp 'let ) (compilation-let arg env fenv nomf))
     ((is-instructionruction exp 'labels) (compilation-labels arg env fenv nomf))
     ((and (consp (car exp)) (eql (caar exp) 'lambda)) (compilation-lambda exp env fenv nomf))
     (`(function ,(car exp)) (compilation-appel exp env fenv nomf))
    )
    )
  )