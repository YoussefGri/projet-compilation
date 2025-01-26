;; Récupération des instructions en mémoire et exécution.

(defun get-memoire-pc (&optional (mv 'mv))
  (get-mem mv (get-pc mv))
  )

(defun set-memoire-pc (mv val)
  (set-mem mv (get-pc mv) val)
  )

(defun get-memoire-lc (&optional (mv 'mv))
  (get-mem mv (get-lc mv))
  )

(defun set-memoire-lc (mv val)
  (set-mem mv (get-lc mv) val) ;; met à l'adresse pointée par LC la valeur val
  )

(defun is-instruction (mv inst)
  (eql (car (get-memoire-pc mv)) inst)
  )

(defun in-fonction (mv)
  (is-instruction mv 'FENTRY)
  )

(defun out-fonction (mv)
  (is-instruction mv 'FEXIT)
  )

(defun saut-fonction (mv nbfun)
  (setf nbfun (+ nbfun 1))
  (loop while (< 0 nbfun)
	do 
	(decremente-pc mv)
	(cond
	 ((in-fonction mv) (setf nbfun (+ nbfun 1)))
	 ((out-fonction mv) (setf nbfun (- nbfun 1)))
	 )
	)
  )

(defun mv-op (mv op src dst)
  (let ((adr (get-destination mv dst))
	(res (apply op (list (get-source mv (get-destination mv dst)) (get-source mv src)))))
    (set-registre mv adr res)
    )
  )

(defun mv-jcond (mv dst cond)
  (if (apply cond (list mv)) 
      (mv-jmp mv dst))
  )