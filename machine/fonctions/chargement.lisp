;; Fonctions de gestion du chargement du code assembleur en mémoire.

(defun is-saut (inst)
  (member (car inst) '(JMP JEQ JL JG JLE JGE JNE))
  )

(defun case-adr (mv exp inst etiqLoc etiqLocNR)
  (if (get-hash etiqLoc (cadadr exp)) ;; verifie si l'etiquette  de l'expression est déjà résolue
      (error "Étiquettes multiples : ~S" (cadr inst))
    (progn ;; sinon
    ;; Ajout de l'étiquette avec l'adresse (LC + 1), car la prochaine instruction sera stockée à cette position.
      (set-hash etiqLoc (cadr inst) (+ (get-lc mv) 1)) 
      (assign-address-to-expressions mv (get-hash etiqLocNR (cadr inst)) (+ (get-lc mv) 1));; parcourt les expressions qui sont encore liées à cette étiquette non résolue et les met à jour avec l'adresse correcte.
      (if (get-hash etiqLocNR (cadr inst))
	  (progn
	    (set-hash etiqLocNR (cadr inst) ())
	    (incremente-hash etiqLocNR) ;; incrémente le compteur d'étiquettes non résolues
	    )
	)
      )
    )
  )

(defun case-varg (mv exp inst)
  (if (null (get-etiq mv (cadr inst)));; s'il n'existe pas d'etiquette pour la variable globale
      (progn
	(set-etiq mv (cadr inst) (get-propriete mv :VP)) ;; ajout de l'etiquette de la variable globale avec la valeur du pointeur de pile (VP)
	(if (< (get-propriete mv :VP) (- (get-propriete mv :BP) 1)) ;; on verifie si on a encore de la place dans la pile pour stocker une variable
	    (incremente-propriete mv :VP);; on positionne le vp sur la prochaine case libre
	  )
	)
    )
  )

(defun case-saut (mv exp inst)
  (if (get-etiq mv (cadadr inst)) ;; si l'etiquette du saut est déjà résolue (associée à une adresse dans la mémoire)
      (set-memoire-lc mv (list (car inst) (get-etiq mv (cadadr inst))));; décalage du lc en y mettant l'adresse associée à l'etiquette ciible
    (progn ;; sinon                                   
      (if (null (get-etiqNR mv (cadadr inst))) ;; verif si l'etiquette est résolue 
	  (incremente-etiqNR mv) ;; si oui en incrémente le compteur d'etiquettes resolues
	)
  ;; Associe l'étiquette non résolue à une liste contenant : 
  ;;    - Le compteur LC courant (l'adresse où se trouve l'instruction de saut non résolue). 
  ;;    - Les données existantes pour cette étiquette non résolue (par exemple, d'autres emplacements où elle est référencée).
      (set-etiqNR mv (cadadr inst) (list* (get-lc mv) (get-etiqNR mv (cadadr inst))))
      (set-memoire-lc mv inst) ;; on stocke l'instruction de saut non résolue dans la mémoire à l'adresse pointée par LC
      )
    )
  (decremente-lc mv);; décrémentation du lc pour passer à l'instruction suivante
  )

(defun case-fonction (mv exp inst)
  (if (get-etiq mv (cadadr exp))
      (error "Fonction déjà définie : ~S" (cadadr exp))
    (progn
      (set-memoire-lc mv inst) ;; stockage de l'instruction de fonction dans la mémoire à l'adresse pointée par LC
      (set-etiq mv (cadadr exp) (get-lc mv)) ;; ajout de l'etiquette à la liste des etiquettes résolues avec l'adresse pointée par LC
      (assign-address-to-expressions mv (get-etiqNR mv (cadadr exp)) (get-lc mv)) ;; mise à jour des expressions liées à l'etiquette non résolue
      (if (get-etiqNR mv (cadadr exp))
	  (progn
	    (set-etiqNR (cadadr exp) ());; on vide la liste des expressions liées à l'etiquette non résolue
	    (incremente-etiqNR mv)
	    )
	)
      (decremente-lc mv)
      )
    )
  )

(defun case-other (mv exp inst etiqLoc etiqLocNR)
  (if (is-saut inst) 
      (progn
	(if (get-hash etiqLoc (cadadr inst)) ;; si l'etiquette du saut est déjà résolue
	    (set-memoire-lc mv (list (car inst) (get-hash etiqLoc (cadadr inst)))) ;; on stocke l'instruction de saut dans la mémoire à l'adresse pointée par LC
	  (progn ;; sinon
	    (if (null (get-hash etiqLocNR (cadadr inst)))
		(incremente-hash etiqLocNR)
	      )
	    (set-hash etiqLocNR (cadadr inst) (list* (get-lc mv) (get-hash etiqLocNR (cadadr inst)))) ;; On ajoute l'adresse actuelle de LC (compteur de chargement) à la liste des instructions qui dépendent de cette étiquette.
	    (set-memoire-lc mv inst) ;; on stocke l'instruction de saut dans la mémoire à l'adresse pointée par LC
	    )
	  )
	);; si l'instruction n'est pas un saut on la stocke directement dans la mémoire à l'adresse pointée par LC
    (set-memoire-lc mv inst)
    )
  (decremente-lc mv)
  )
