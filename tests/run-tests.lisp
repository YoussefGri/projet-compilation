;; Fonctions de test.

(defun run-test-case (mv val code aff) ;; mv : machine virtuelle, val : valeur attendue, code : code à tester, aff : une char affichée avec le test (nom)
  (load-machine mv (compilation code)) ;; on charge le code compilé dans la vm
  (if (eql val (run-machine mv)) ;; si la valeur contenue dans R0 après execution de la vm correspond bien à la valeur attendue
      (format t "OK : ~S : ~S = ~D~%" aff code val) ;; si oui on affiche ok avec le nom du test, le code et la valeur attendue
    (format t "KO : ~S : ~S <> ~D~%" aff code val) ;; sinon KO ...
    )
  )

(defun run-test-fonction (mv val code fun aff) ;; ici code sera l'appel de la fonction (fact n) par exemple, fun sera la définition de la fonction (defun fact (n) ...)
  (let ((compiled-fun (compilation fun))
        (compiled-code (compilation code)))
    (load-machine mv compiled-fun) ;; on charge l'appel compilé de la fonction dans la vm
    (load-machine mv compiled-code) ;; On charge le corps de la fonction compilé dans la vm
    (format t "F  : ~S~%" fun) ;; F pour fonction
    (format t "Compiled Function Body: ~S~%" compiled-fun) ;; Affichage du code compilé de la fonction
    (format t "Compiled Function Call: ~S~%" compiled-code) ;; Affichage du code compilé de l'appel
    (if (eql val (time (run-machine mv))) ;; time va nous afficher le temps mis par la vm pour executer le code. on récupère en même temps la valeur de retour de la vm également
        (format t "OK : ~S : ~S = ~D~%~%" aff code val)
      (format t "KO : ~S : ~S <> ~D~%~% ~S" aff code val (run-machine mv))
      )
    )
  )

;; Version de run-test-fonction sans l'affichage du code assembleur compilé

; (defun run-test-fonction (mv val code fun aff) ;; ici code sera l'appel de la fonction (fact n) par exemple, fun sera la définition de la fonction (defun fact (n) ...)
;   (load-machine mv (compilation fun)) ;; on charge l'appel compilé de la fonction dans la vm
;   (load-machine mv (compilation code)) ;; On charge le corps de la fonction compilé dans la vm
;   (format t "F  : ~S~%" fun) ;; F pour fonction
;   (if (eql val (time (run-machine mv))) ;; time va nous afficher le temps mis par la vm pour executer le code. on récupère en même temps la valeur de retour de la vm également
;       (format t "OK : ~S : ~S = ~D~%~%" aff code val)
;     (format t "KO : ~S : ~S <> ~D~%~% ~S" aff code val (run-machine mv))
;     )
;   )

(defun run-test-case-cond (mv val code aff)
  (load-machine mv (compilation code)) ;; on charge le code compilé correspondant à la condition dans la vm (ex. (if (< 1 2) 1 0) ou (cond ((< 1 2) 1) ((> 1 2) 0) (t 2)))
  (run-machine mv) ;; on lance l'execution de la vm
  (if (or (and val (get-registre mv :R0)) (and (not val) (not (get-registre mv :R0)))) 
  ;; si la valeur attendue est vraie et que la valeur de retour de la vm est vraie ou si la valeur attendue est fausse et que la valeur de retour de la vm est fausse
      (format t "OK : ~S : ~S = ~D~%" aff code val)
    (format t "KO : ~S : ~S <> ~D~%" aff code val)
    )
  )

(defun run-test-case-double (mv val code bis aff)
  (load-machine mv (compilation bis)) ;; Charge l'initialisation (ex. setf)
  (load-machine mv (compilation code)) ;; Charge l'opération à tester sur la variable initialisée
  (format t "INIT  : ~S~%" bis) ;; Affiche l'initialisation de la variable
  (if (eql val (run-machine mv))
      (format t "OK : ~S : ~S = ~D~%" aff code val)
    (format t "KO : ~S : ~S <> ~D~%" aff code val)
    )
  )