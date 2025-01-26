;; Manipulations sur les tables de hashage des étiquettes.

(defun get-hash (tab cle) 
  (gethash cle tab) ;; recup de la valeur associée à une clé dans la table de hashage
  )

(defun set-hash (tab cle val)
  (setf (gethash cle tab) val) ;; affectation d'une valeur à une clé dans la table de hashage
  )

(defun incremente-hash (tab)
  (set-hash tab 'nb (+ (get-hash tab 'nb) 1)) ;; incrémentation de la valeur associée à la clé 'nb' dans la table de hashage
  )

(defun get-etiq (mv cle)
  (get-hash (get-propriete mv :etiq) cle) ;; recup de la valeur associée à une clé dans la table de hashage des étiquettes resolues
  )

(defun set-etiq (mv cle val)
  (set-hash (get-propriete mv :etiq) cle val)
  )

(defun incremente-etiq (mv)
  (incremente-hash (get-propriete mv :etiq))
  )

(defun get-etiqNR (mv cle)
  (get-hash (get-propriete mv :etiqNR) cle) ;; recup de la valeur associée à une clé dans la table de hashage des étiquettes non résolues
  )

(defun set-etiqNR (mv cle val)
  (set-hash (get-propriete mv :etiqNR) cle val)
  )

(defun incremente-etiqNR (mv)
  (incremente-hash (get-propriete mv :etiqNR))
  )

(defun assign-address-to-expressions (mv exp adr) ;; parcourt l'expression et résout
  (if (null exp) 
      ()
    (progn
      (set-mem mv (car exp) (list (car (get-mem mv (car exp))) adr)) ;; Cette ligne met à jour la mémoire pour chaque élément de l'expression (car exp), en associant une adresse (adr) aux valeurs qui étaient initialement liées à cet élément.
      (assign-address-to-expressions mv (cdr exp) adr)
      )
    )
  )

