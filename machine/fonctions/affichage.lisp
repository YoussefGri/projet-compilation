;; Fonctions d'affichage du contenu de la mémoire.


;; Affiche une zone spécifique de la mémoire.
(defun affichage-zone (mv ind fin)
  ;; Vérifie que l'indice actuel `ind` est dans la plage valide :
  ;; 1. `ind` est inférieur ou égal à la limite `fin`.
  ;; 2. `ind` est strictement inférieur à la taille totale de la mémoire (`get-taille mv`).
  ;; 3. `ind` est positif ou nul.
  (if (and (<= ind fin) (< ind (get-taille mv)) (>= ind 0))
      ;; Si les conditions sont respectées :
      (progn 
      ;; Affiche la case actuelle de l'indice `ind`.
      (affichage-case mv ind)
        ;; Appelle récursivement `affichage-zone` pour l'indice suivant (`ind + 1`).
         (affichage-zone mv (+ ind 1) fin))
    ;; Sinon, ne fait rien (fin de la récursion).
    )
  )

;; Affiche le contenu d'une case mémoire à l'indice donné.
(defun affichage-case (mv ind)
  ;; Si l'indice est un multiple de 5, affiche un retour à la ligne (5 a fin d'avoir une bonne vision)
  ;; suivi de l'indice de la case.
  (if (= (mod ind 5) 0) (format t "~%~D " ind))
  ;; Affiche le contenu de la case mémoire à l'indice donné.
  (format t ".~S" (get-mem mv ind))
  )

;; Affiche les 100 premières cases de la mémoire globale.
(defun affichage-globals (&optional (mv 'mv))
  ;; Appelle `affichage-zone` pour afficher les indices de 0 à 100 inclus.
  (affichage-zone mv 0 100)
  )
;; Affiche toutes les cases de la mémoire (jusqu'à `get-taille mv`).
(defun aff-all (&optional (mv 'mv))
  ;; Appelle `affichage-zone` pour afficher les indices de 0
  ;; jusqu'à la taille totale de la mémoire (`get-taille mv`).
  (affichage-zone mv 0 (get-taille mv))
  )