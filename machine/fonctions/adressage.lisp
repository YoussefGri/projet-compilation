;; Fonctions de résolution des adresses.


;; Exemples d'adressage dans l'ordre :
;; (), 598, :R3, (:DIESE 5), (6 :R1), (:@ :etiqfin), (:* 2 :R2)


;; Vérifie si l'élément donné est un registre ou un pointeur prédéfini.
(defun is-prop (e) 
  ;; Vérifie si l'élément `e` appartient à une liste contenant les registres :R0, :R1, etc.
  (member e (list :R0 :R1 :R2 :R3 :SP :VP :FP :DPP :DE :DPG :PC :LC))
  ;; Retourne `t` (true ) si `e` est trouvé dans la liste, sinon retourne `nil`.
  )

;; Vérifie si l'élément est une constante.
(defun is-constante (e)
  (eql (car e) :DIESE)
  )

;; Vérifie si l'élément est un index (nombre).
(defun is-index (e)
  (numberp (car e))
  )

;; Récupère une valeur mémoire à un index calculé.
(defun get-index (mv e)
;; Récupère la valeur mémoire à une adresse calculée.
  ;; `(car e)` est l'index de base.
  ;; `(get-source mv (cadr e))` ajoute une valeur supplémentaire provenant d'une autre source.
  (get-mem mv (+ (car e) (get-source mv (cadr e))))
  )


;; Vérifie si l'élément est une étiquette (référence symbolique).
(defun is-etiq (e) 
  ;; Teste si le premier élément de `e` est le symbole `:@`, indiquant une étiquette.
  (eql (car e) :@)
  )


;;un élément indirect  est une manière d'accéder à une valeur ou à une adresse mémoire en passant par une autre adresse

;; Vérifie si l'élément est un accès indirect (accès via une adresse mémoire).
(defun is-indir (e) 
  ;; Teste si le premier élément de `e` est le symbole `:*`, indiquant un accès indirect.
  (eql (car e) :*)
  ;; Retourne `t` si c'est un accès indirect, sinon `nil`.
  )



;; Récupère la valeur d'une source indirecte.
(defun get-indir-src (mv e)
;; Si la liste `e` contient plus de deux éléments (accessoires),
  ;; calcule la valeur de la source en tenant compte du deuxième élément.
  (get-mem mv (if (null (cddr e)) 
                  ;; Récupère directement la source pour le deuxième élément.
		  (get-source mv (cadr e)) 
                ;; Sinon, considère toute la liste sauf le premier élément.
		(get-source mv (cdr e)))
	   )
  )



;; Récupère l'adresse mémoire d'une destination indirecte.
(defun get-indir-dst (mv e)
  ;; Si la liste `e` ne contient pas plus de deux éléments (cddr retourne nil).
  (if (null (cddr e))
      ;; Récupère directement la source pour le deuxième élément.
      (get-source mv (cadr e))
    ;; Sinon, vérifie si le reste de la liste représente un index.
    (if (is-index (cdr e))
	;; Calcule l'adresse en ajoutant le deuxième élément et la source obtenue à partir du troisième élément.
	(+ (cadr e) (get-source mv (caddr e)))
      ;; Sinon, vérifie si le reste de la liste représente une étiquette.
      (if (is-etiq (cdr e)) 
	  ;; Récupère la source pour l'étiquette.
	  (get-source mv (cdr e))
	)
      )
    )
  )
;; Vérifie si l'élément représente une variable locale.
(defun is-local (e)
  ;; Vérifie si le premier élément de `e` est égal à `LOC`.
  (eql (car e) 'LOC)
  )


;; Récupère une nouvelle valeur pour le registre FP (Frame Pointer) en fonction d'une structure d'appel.

(defun get-newFP (mv e) 
 ;; Initialise `newFP` à la valeur actuelle de FP dans `mv`.
  (let ((newFP (get-fp mv)))
    (loop while (> (get-mem mv (+ 3 newFP)) (caddr e)) ;; on remonte dans la pile jusqu'à trouver le FP de la fonction appelante (même niveau d'encapsulation)
	  do (setf newFP (get-mem mv (+ 2 newFP))) ;; acces a l'ancien FR
	  )
    ;; Retourne la valeur calculée pour FP.
    newFP
    )
  )

;; Récupère une source locale à partir d'un contexte mémoire.
(defun get-local-source (mv e)
  ;; Si le troisième élément de `e` est nul, le définit à 0.
  (if (eql (caddr e) ()) (setf (caddr e) 0))
  ;; Calcule une nouvelle valeur pour FP.
  (let ((newFP (get-newFP mv e)))
    ;; Vérifie si l'offset est négatif.
    (if (< (cadr e) 0) ;; si l'offset est négatif on est dans le cas d'arguments sinon on est dans le cas de variables locales
	(get-mem mv (- newFP (get-mem mv newFP) 1 (cadr e)))
      (get-mem mv ( + 4 newFP (cadr e))) ;; on ajoute 4 pour récupèrer la valeur de la cadr e ième var loc
      )
    )
  )

;; Récupère une destination locale en fonction d'un contexte mémoire.
(defun get-local-destination (mv e)
  ;; Calcule une nouvelle valeur pour FP.
  (let ((newFP (get-newFP mv e)))
    ;; Vérifie si l'offset est négatif.
    (if (< (cadr e) 0)
	;; Calcule l'adresse pour un offset négatif.
	(- newFP (get-mem mv newFP) 1 (cadr e))  
      ;; Sinon, calcule l'adresse pour un offset positif.
      (+  4 newFP  (cadr e))
      )
    )
  )


;; Renvoie les contenus pour src / les adresses pour dst.
 
;; Récupère la valeur contenue dans une source.
(defun get-source (mv exp)
  (if (atom exp)
  ;; Si l'expression est un atome.
      (cond
       ;; Si l'expression est nulle, retourne 0.
       ((null exp) 0)
       ;; Si c'est un nombre, retourne la valeur mémoire correspondante.
       ((numberp exp) (get-mem mv exp))
       ;; Si c'est une propriété (registre, etc.), récupère sa valeur.
       ((is-prop exp) (get-propriete mv exp))
       )
    ;; Si l'expression est une liste (non-atome).
    (if (consp exp)
	(cond
	 ;; Si c'est une constante, retourne sa valeur.
	 ((is-constante exp) (cadr exp))
	 ;; Si c'est un index, appelle `get-index` pour obtenir la valeur.
	 ((is-index exp) (get-index mv exp))
	 ;; Si c'est une étiquette, récupère l'adresse associée.
	 ((is-etiq exp) (get-etiq mv (cadr exp))) 
	 ;; Si c'est une adresse indirecte, récupère la source indirecte.
	 ((is-indir exp) (get-indir-src mv exp))
	 ;; Si c'est une variable locale, récupère la source locale.
	 ((is-local exp) (get-local-source mv exp))
	 )
      )
    )
  )

;; Récupère l'adresse d'une destination.
(defun get-destination (mv exp)
  (if (atom exp)
  ;; Si l'expression est un atome.
      (cond
       ;; Si l'expression est nulle, retourne 0.
       ((null exp) 0)
       ;; Si c'est un nombre, retourne directement l'adresse.
       ((numberp exp) exp)
       ;; Si c'est une propriété, retourne directement son adresse.
       ((is-prop exp) exp)
       )
    (if (consp exp)
    ;; Si l'expression est une liste (non-atome).
	(cond
	 ;; Si c'est un index, retourne directement l'adresse.
	 ((is-index exp) exp)
	 ;; Si c'est une étiquette, récupère l'adresse associée.
	 ((is-etiq exp) (get-etiq mv (cadr exp)))
	 ;; Si c'est une constante, retourne directement sa valeur.
	 ((is-constante exp) (cadr exp))
	 ;; Si c'est une adresse indirecte, calcule la destination indirecte.
	 ((is-indir exp) (get-indir-dst mv exp))
	 ;; Si c'est une variable locale, calcule la destination locale.
	 ((is-local exp) (get-local-destination mv exp))
	 )
      )
    )
  )