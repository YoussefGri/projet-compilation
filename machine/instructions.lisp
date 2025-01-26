;; Instructions utilisables par la machine virtuelle.
;;
;; MOVE source cible
;;
;; PUSH source
;; POP cible
;;
;; ADD source cible
;; SUB source cible
;; MULT source cible
;; DIV source cible
;;
;; INC cible
;; DEC cible
;;
;; JMP cible
;; JSR etiquette
;;
;; CMP un deux
;; JL cible
;; JEQ cible
;; JG cible
;; JLE cible
;; JGE cible
;; JNE cible
;;
;; RTN
;; NOP
;; ERR


;; ********** Instructions de transfert registres <-> mémoire.

(defun mv-move (mv src dst)
  (let ((adr (get-destination mv dst))  ;; Récupère l'adresse de destination.
	(res (get-source mv src)))  ;; Récupère la valeur de la source.
    (if (numberp adr)      ;; Si l'adresse de destination est un nombre (un emplacement mémoire)...adr récupère l'adresse de la destination où la valeur doit être déplacée.
	(set-mem mv adr res)     ;; Enregistrer la valeur à cette adresse mémoire. res récupère la valeur qui provient de la source.
      (set-propriete mv adr res) ;; Sinon, définir la valeur d'une propriété (si la destination est une variable).
      )
    )
  )


;; ********** Instructions de gestion de la pile.

(defun mv-push (mv src)
  (incremente-sp mv)   ;; Incrémente le pointeur de pile (SP) pour préparer l'empilement.
  (mv-move mv src '(:* :SP))  ;; Déplace la valeur source dans la pile, à l'adresse pointée par SP.
  )

(defun mv-pop (mv dst)
  (mv-move mv '(:* :SP) dst) ;; Déplace la valeur à l'adresse pointée par SP dans la destination (dst).
  (decremente-sp mv)   ;; Décrémente le pointeur de pile (SP) après l'empilement.
  )

;; ********** Instructions arithmétiques entre registres.

(defun mv-add (mv src dst)   ;; Ajouter la valeur de src à celle de dst et stocker le résultat dans dst
  (mv-op mv '+ src dst)  ;; Effectuer l'addition entre les valeurs de src et dst
  )

(defun mv-sub (mv src dst) ;; Soustraire la valeur de src de celle de dst et stocker le résultat dans dst
  (mv-op mv '- src dst)   ;; Effectuer la soustraction entre les valeurs de dst et src
  )

(defun mv-mult (mv src dst) ;; Multiplier la valeur de src par celle de dst et stocker le résultat dans dst
  (mv-op mv '* src dst)    ;; Effectuer la multiplication entre les valeurs de src et dst
  )

(defun mv-div (mv src dst)  ;;  Diviser la valeur de dst par celle de src et stocker le résultat dans dst
  (mv-op mv '/ src dst)  ;; Effectuer la division entre les valeurs de dst et src
  )


;; ********** Instructions arithmétiques registre <-> valeur.

(defun mv-inc (mv dst)  ;; Incrémenter la valeur du registre dst de 1
  (mv-add mv '(:DIESE 1) dst) ;; Ajouter 1 à la valeur de dst
  )

(defun mv-dec (mv dst)  ;; Décrémenter la valeur du registre dst de 1
  (mv-sub mv '(:DIESE 1) dst)  ;; Soustraire 1 de la valeur de dst
  )

;; ********** Instructions de saut.

(defun mv-jmp (mv dst) ;;  Sauter à l'adresse spécifiée par dst
  (if (numberp dst)  
      (set-pc mv dst) ;; Si dst est un nombre, on met à jour le compteur de programme (PC)
    (mv-move mv dest :PC) ;; Sinon, on déplace la valeur de dst dans le registre PC
    )
  )

(defun mv-jsr (mv etq) ;;  Sauvegarder l'adresse de retour dans la pile et sauter à l'étiquette spécifiée par etq
  (mv-push mv :PC)   ;; Empile l'adresse de retour (PC)
  (mv-jmp mv etq)   ;; Effectue un saut à l'étiquette spécifiée par etq
  ) 


;; ********** Instructions de saut conditionnel.

(defun mv-cmp (mv recto verso) ;; Comparer les valeurs de recto et verso et mettre à jour les drapeaux en conséquence
  (let ((r (get-source mv recto)) ;; Récupère la valeur de recto
	(v (get-source mv verso)))       ;; Récupère la valeur de verso
    (if (and (numberp r) (numberp v))  ;; Vérifie si les deux valeurs sont des nombres
	(cond
	 ((eql r v) (set-drapeaux mv 0 1 0)) ;; Si les valeurs sont égales, met à jour les drapeaux pour "égal"
	 ((< r v) (set-drapeaux mv 1 0 0))   ;; Si r est plus petit que v, met à jour les drapeaux pour "plus petit"
	 ((> r v) (set-drapeaux mv 0 0 1))   ;; Si r est plus grand que v, met à jour les drapeaux pour "plus grand"
	 )
      (if (eql r v)   ;; Si les valeurs ne sont pas des nombres mais sont égales
	  (set-drapeaux mv 0 1 0)  ;; Met à jour les drapeaux pour "égal"
	(set-drapeaux mv 0 0 0)  ;; Sinon, met les drapeaux à zéro
	)
      )
    )
  )

(defun mv-jl (mv dst)  ;; Sauter à dst si la condition "plus petit" est remplie
  (mv-jcond mv dst 'est-pluspetit)  ;; Utilise la fonction mv-jcond pour vérifier si la condition "plus petit" est remplie
  )

(defun mv-jeq (mv dst) ;; Sauter à dst si la condition "égal" est remplie
  (mv-jcond mv dst 'est-egale) ;; Utilise la fonction mv-jcond pour vérifier si la condition "égal" est remplie
  )
      
(defun mv-jg (mv dst) ;;Sauter à dst si la condition "plus grand" est remplie
  (mv-jcond mv dst 'est-plusgrand)  ;; Utilise la fonction mv-jcond pour vérifier si la condition "plus grand" est remplie
  )

(defun mv-jle (mv dst) ;; Sauter à dst si la condition "non plus grand" est remplie
  (mv-jcond mv dst 'pas-plusgrand) ;; Utilise la fonction mv-jcond pour vérifier si la condition "non plus grand" est remplie
  )


(defun mv-jge (mv dst) ;; Sauter à dst si la condition "non plus petit" est remplie
  (mv-jcond mv dst 'pas-pluspetit) ;; Utilise la fonction mv-jcond pour vérifier si la condition "non plus petit" est remplie
  )

(defun mv-jne (mv dst) ;; Sauter à dst si la condition "non égal" est remplie
  (mv-jcond mv dst 'pas-egale) ;; Utilise la fonction mv-jcond pour vérifier si la condition "non égal" est remplie
  )


;; ********** Instructions de gestion de la machine.

(defun mv-rtn (mv) ;;  Effectuer un retour d'une fonction (restaurer les registres de pile et de programme)
  (mv-move mv '( 1 :FP) :SP) ;; Déplace la valeur de FP dans SP pour restaurer le pointeur de pile(fp+1)
  (mv-move mv '( 4 :FP) :PC) ;; Déplace la valeur de FP dans PC pour restaurer l'adresse de retour pour reprendre l'execution
  (mv-move mv '( 2 :FP)  :FP) ;; Déplace la valeur de FP pour restaurer le pointeur de pile
  )

(defun mv-nop (mv)  ;;  Ne rien faire (pas d'opération)
  )

(defun mv-err (mv exp) ;; Afficher un message d'erreur avec l'expression spécifiée
  (format t "Erreur : ~S~%" exp) ;; Affiche l'erreur avec l'expression passée en argument
  )