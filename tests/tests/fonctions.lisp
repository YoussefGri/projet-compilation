(format t "~%Tests des fonctions et lambda-expressions :~%")

(run-test-case-double mv 5 '(f 6) '(defun f (x) (- x 1)) "FUN")

(run-test-case mv 22 '((lambda (x) (* 2 x)) 11) "LAMBDA")
