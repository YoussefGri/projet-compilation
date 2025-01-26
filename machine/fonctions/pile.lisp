;; Fonctions pour manipuler la pile.



;; Récupère la valeur de la pile (Stack Pointer) de la machine virtuelle.
(defun get-sp (&optional (mv 'mv))
  (get-propriete mv :SP)  
                          ;; Appelle la fonction `get-propriete` pour obtenir la valeur de la propriété :SP.
                            ;; Si aucune valeur n'est fournie pour `mv`, la valeur par défaut est `'mv`.
  )
;; Modifie la valeur du pointeur de la pile (Stack Pointer) de la machine virtuelle.
(defun set-sp (mv val)
  (set-propriete mv :SP val)   ;; Appelle la fonction `set-propriete` pour définir la propriété :SP à la valeur `val` fournie.
)

;; Incrémente la valeur du pointeur de la pile (Stack Pointer).
(defun incremente-sp (mv)
  (incremente-propriete mv :SP);; Appelle la fonction `incremente-propriete` pour augmenter la valeur de la propriété :SP de 1.
  )
;; Décrémente la valeur du pointeur de la pile (Stack Pointer).
(defun decremente-sp (mv)
  (decremente-propriete mv :SP)   ;; Appelle la fonction `decremente-propriete` pour diminuer la valeur de la propriété :SP de 1.
  )
;; Récupère la valeur du pointeur de cadre (Frame Pointer) de la machine virtuelle.
(defun get-fp (&optional (mv 'mv))
  (get-propriete mv :FP)   ;; Appelle la fonction `get-propriete` pour obtenir la valeur de la propriété :FP (Frame Pointer).
  )
 
;; Modifie la valeur du pointeur de cadre (Frame Pointer) de la machine virtuelle.
(defun set-fp (mv val)
  (set-propriete mv :FP val)
) 