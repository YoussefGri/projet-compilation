;; Gestion de l'exécution de la machine.




;; Fonction pour vérifier si l'instruction en cours est valide (présente dans la mémoire).

(defun inst-ok (mv)
  (consp (get-memoire-pc mv))   ;; Vérifie si le compteur de programme (PC) pointe vers une liste d'instruction valide.
  )

;; Fonction pour vérifier si l'instruction en cours n'est pas un HALT.

(defun not-halt (mv)
  (not (is-instruction mv 'HALT))
  )

;; Fonction pour vérifier si la machine virtuelle est en cours d'exécution.

(defun mv-running (&optional (mv 'mv))
  (and (inst-ok mv)  (not-halt mv))
  )

;; Fonction pour vérifier s'il y a un dépassement de pile dans la machine virtuelle.

(defun mv-overflow (mv)
  (>= (get-sp mv) (get-pc mv))
  )