(format t "~%Tests des structures de contrôle :~%")

(run-test-case-double mv 10 '(* a 2) '(setf a 5) "SETF")

(run-test-case mv 18 '(progn (setf a 9) (if (< a 10) (* 2 a) (* 3 a))) "PROGN")

(run-test-case-double mv 24 '(test-let-bis 3 4) '(defun test-let-bis (x y) (let (( a 1) (b 2)) (* x y a b))) "LET")

