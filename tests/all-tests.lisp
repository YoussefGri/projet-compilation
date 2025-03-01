(require "tests/run-tests.lisp")

;; Création d'une machine virtuelle dédiée aux tests.

(setf mv 'mvtest)
(make-machine mv 5000 T)

;; Tests de fonctions fibonacci et factorielle.

 (require "tests/tests/fibo_fact.lisp")

;;Tests opérateurs arith.

; (require "tests/tests/arithmetique.lisp")

; ;; Tests d'opérateurs booléens.

; ; (require "tests/tests/booleens.lisp")

; ;; Tests de boucles de condition.

; (require "tests/tests/conditions.lisp")

; ;; Tests de fonctions.

; (require "tests/tests/fonctions.lisp")

; ;; Tests de structures de contrôle.

; (require "tests/tests/structures.lisp")

