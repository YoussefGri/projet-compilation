(format t "~%Tests de calculs arithmétiques :~%")

(run-test-case mv 108 '(+ 65 43) "ADD")
(run-test-case mv -45 '(- 55 100) "SUB")
(run-test-case mv 64 '(* 8 8) "MULT")
(run-test-case mv 1 '(/ 8 8) "DIV")

(run-test-case mv -26400 '(* (/ 70 (- 5699 5692)) (- 2 4) 33 40) "ADD SUB MULT DIV")