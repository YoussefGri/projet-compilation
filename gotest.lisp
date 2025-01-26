(require "machine/machine.lisp")

(format t "~%***** Composant machine virtuelle chargé.~%~%")

(require "compilateur/compilateur.lisp")

(format t "~%***** Composant compilateur chargé.~%~%")

(format t "~%***** Lancement des tests.~%~%")

(require "tests/all-tests.lisp")

