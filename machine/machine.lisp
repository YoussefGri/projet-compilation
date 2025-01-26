(require "machine/all-fonctions.lisp")
(require "machine/instructions.lisp")

;; Fonctions structurelles de la machine virtuelle.

;; nom : Nom de la machine.
;; R0, R1, R2, R3 : Registres. Ce sont des propriétés de la machine virtuelle, donc pas de position mémoire.
;; BP : Base Pointer initialisé à 100, pile montante donc taille de pile c'est taille memoire - 100.
;; SP : Stack Pointer, si pile vide SP = BP. SP pointe sur la prochaine case libre de la pile, incrementé à chaque push.
;; FP : Frame Pointer
;; DPP : Drapeau de comparaison "plus petit".
;; DE : Drapeau de comparaison "égalité".
;; DPG : Drapeau de comparaison "plus grand".
;; taille : Taille allouée à la mémoire (pile + tas + code).
;; memtab : Mémoire de la machine.
;; PC : Program Counter, compteur ordinal, position dans le code.
;; LC : Load Counter, position du chargement du code. initialisé tout en haut, la lecture du code se fait du bas vers le haut. le chargement du code se fait de la fin vers le début.
;; etiq : Table de hashage pour les étiquettes.
;; etiqNR : Table de hashage des étiquettes non résolues



;; ********** Création d'une machine virtuelle.

(defun make-machine (&optional (nom 'mv) (tmem 10000) (aff ()))
  (set-propriete nom :nom nom)
  (set-propriete nom :R0 0)
  (set-propriete nom :R1 0)
  (set-propriete nom :R2 0)
  (set-propriete nom :R3 0)
  (set-propriete nom :BP 100)
  (set-propriete nom :SP 100)
  (set-propriete nom :VP 1)
  (set-propriete nom :FP 0)
  (set-propriete nom :DPP 0)
  (set-propriete nom :DE 0)
  (set-propriete nom :DPG 0)
  (reset-memoire nom tmem)
  (if (not (null aff)) (print-machine nom))
  )
  
;; ********** Vidage mémoire d'une machine virtuelle.

(defun reset-memoire (&optional (nom 'mv) (tmem 10000))
  (let ((taille (max tmem 1000)))
    (set-taille nom taille)
    (set-propriete nom :memtab (make-array taille))
    (set-pc nom (- taille 1))
    (set-lc nom (- taille 1))
    (set-propriete nom :etiq (make-hash-table :size taille))
    (set-propriete nom :etiqNR (make-hash-table :size taille))
    (set-etiqNR nom 'nb 0)
    )
  )


;; ********** Chargement de code dans la mémoire d'une machine virtuelle.

(defun load-machine (mv asm)
  (let ((exp asm)
	(inst (car asm));; lecture première instruction du code assembleur
	(etiqLoc (make-hash-table :size (get-taille mv))) ;; table de hashage des étiquettes locales RESOLUES
	(etiqLocNR (make-hash-table :size (get-taille mv)))) ;; table de hashage des étiquettes locales NON RESOLUES
    (set-hash etiqLocNR 'nb 0) ;; compteur d'étiquettes non résolues init à 0
    (loop while exp ;; tant qu'il y a des choses à lire dans l'expression
	  do
	  (case (car inst) ;; lecture du prmier element de l'instruction (adresse, variable globale, saut, fonction ou autres)
	    ('@ (case-adr mv exp inst etiqLoc etiqLocNR));; exemple inst : (@ labelA) etiquette
	    ('VARG (case-varg mv exp inst))
	    ('JSR (case-saut mv exp inst)) ;; saut vers etiquette (jsr (@ labelA)), appels de fonction
	    ('FEntry (case-fonction mv exp inst))
	    (otherwise (case-other mv exp inst etiqLoc etiqLocNR)) ;; cas par défaut
	    )
	  do (setf exp (cdr exp)) ;; expression suivante (ce qui reste de l'expression précedente)
	  do (setf inst (car exp)) ;; lecture de la première instruction de ce qui reste de l'expression
	  )
    )
  )

  
;; ********** Lancement d'une machine virtuelle.
  
(defun run-machine (&optional (nom 'mv) (aff ()))
  (set-memoire-lc nom '(HALT))
  (let ((nbfun 0))
  (loop while (mv-running nom)
	do
	(if (in-fonction nom) 
	    (saut-fonction nom nbfun)
	  (exec-inst nom (get-memoire-pc nom) aff) 
	  )
	)
 (print-machine nom)
  )
  (if (mv-overflow nom) 
      (error "Débordement de pile")
    (get-registre nom :R0))
  )

(defun exec-inst (mv exp &optional (aff ()))
  (let ((inst (car exp));; Récupère le premier élément de l'expression, l'instruction.
	(param (cadr exp))  ;; Récupère le deuxième élément, le premier paramètre.
	(param-bis (caddr exp))) ;; Récupère le troisième élément, le second paramètre.
    (if (null exp)  ;; Si l'expression est vide, effectuer une instruction 'nop'.
	(mv-nop mv)
      (case inst      ;; Selon l'instruction j'appelle la fonction qui lui est attribué 
	('MOVE (mv-move mv param param-bis))
	('ADD (mv-add mv param param-bis))
	('SUB (mv-sub mv param param-bis))
	('MULT (mv-mult mv param param-bis))
	('DIV (mv-div mv param param-bis))
	('PUSH (mv-push mv param))
	('POP (mv-pop mv param))
	('INCR (mv-inc mv param))
	('DECR (mv-dec mv param))
	('JMP  (mv-jmp mv param))
	('CMP  (mv-cmp mv param param-bis))
	('JEQ (mv-jeq mv param))
	('JL (mv-jl mv param))
	('JLE (mv-jle mv param))
	('JG (mv-jg mv param))
	('JGE (mv-jge mv param))
	('JNE (mv-jne  mv param))
	('JSR (mv-jsr mv param))
	('RTN (mv-rtn mv))
	('FENTRY (mv-nop mv)) ;; FENTRY est une instruction qui ne fait rien (NOP).
	('FEXIT (mv-nop mv))  ;; FEXIT est une instruction qui ne fait rien (NOP).
	('ERR (mv-err mv))   ;; Si l'instruction est ERR, affiche une erreur.
	(otherwise (mv-err mv exp))  ;; Si l'instruction n'est pas reconnue, affiche une erreur.
	)
      )
    (if (not (null aff)) (format t "~S~%" (get-memoire-pc mv)))   ;; Si l'argument 'aff' est fourni, affiche l'état du programme contre le compteur.
    (decremente-pc mv)  ;; Décrémente le compteur de programme pour passer à l'instruction suivante.
    )
  )


;; ********** Affichage de tous les paramètres d'une machine virtuelle.

(defun print-machine (&optional (nom 'mv))
  (format t "~%Machine virtuelle : ~%--- Nom : ~S ~%--- Taille : ~D" nom (get-taille nom))
  (format t "~%- Registres : ~%--- R0 : ~D ~%--- R1 : ~D ~%--- R2 : ~D ~%--- R3 : ~D" 
	  (get-registre nom :R0) (get-registre nom :R1) (get-registre nom :R2) (get-registre nom :R3))
  (format t "~%- Pointeurs : ~%--- BP : ~D ~%--- SP : ~D ~%--- VP : ~D ~%--- FP : ~D"
	  (get-propriete nom :BP) (get-propriete nom :SP) (get-propriete nom :VP) (get-propriete nom :FP))
  (format t "~%- Drapeaux : ~%--- DPP : ~D ~%--- DE : ~D ~%--- DPG : ~D"
	  (get-propriete nom :DPP) (get-propriete nom :DE) (get-propriete nom :DPG))
  (format t "~%- Compteurs : ~%--- PC : ~D ~%--- LC : ~D ~%"
	  (get-pc nom) (get-lc nom))
  )
