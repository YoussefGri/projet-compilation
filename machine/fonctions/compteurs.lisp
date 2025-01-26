;; Manipulation des compteurs PC et LC de la machine virtuelle.

(defun get-pc (&optional (mv 'mv))
  (get-propriete mv :PC)
  )

(defun set-pc (mv val)
  (set-propriete mv :PC val)
)

(defun incremente-pc (mv)
  (incremente-propriete mv :PC)
  )

(defun decremente-pc (mv)
  (decremente-propriete mv :PC)
  )

(defun get-lc (&optional (mv 'mv))
  (get-propriete mv :LC)
  )

(defun set-lc (mv val)
  (set-propriete mv :LC val)
)

(defun incremente-lc (mv)
  (incremente-propriete mv :LC)
  )

(defun decremente-lc (mv)
  (decremente-propriete mv :LC)
  )