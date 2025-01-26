;; Fonctions de mise à jour des drapeaux de la machine virtuelle.



;; DPP drapeau plus petit , DE drapeau == , DPG plus grand 


;;Activé : le drapeau (flag) a une valeur spécifique, généralement 1
;; Pas activé (desacivé ) : le drapeau a une valeur 0 .


;;Indicateurs de conditions : Les drapeaux permettent de savoir si certaines conditions ont été remplies lors d'opérations arithmétiques,
;; logiques ou de comparaison. 

(defun set-drapeaux (mv dpp de dpg)
  (set-propriete mv :DPP dpp)
  (set-propriete mv :DE de)
  (set-propriete mv :DPG dpg)   ;; Met à jour la propriété DPG (drapeau "plus grand").
  )

(defun est-egale (mv)
  (eql (get-propriete mv :DE) 1)   ;; Retourne `t` si la valeur du drapeau DE (égalité) est égale à 1.
  )

(defun pas-egale (mv)
  (not (est-egale mv))   ;; Retourne `t` si le drapeau DE (égalité) n'est pas activé.
  )

(defun est-pluspetit (mv)
  (eql (get-propriete mv :DPP) 1)   ;; Retourne `t` si la valeur du drapeau DPP (plus petit) est égale à 1.
  )

(defun pas-pluspetit (mv)
  (not (est-pluspetit mv))   ;; Retourne `t` si le drapeau DPP (plus petit) n'est pas activé.
  )


;; Vérifie si le drapeau "plus grand" est activé.
(defun est-plusgrand (mv)
  (eql (get-propriete mv :DPG) 1)   ;; Retourne `t` si la valeur du drapeau DPG (plus grand) est égale à 1.
  )


;; Vérifie si le drapeau "plus grand" est désactivé.
(defun pas-plusgrand (mv)
  (not (est-plusgrand mv))     ;; Retourne `t` si le drapeau DPG (plus grand) n'est pas activé.
  )