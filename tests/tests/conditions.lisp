(format t "~%Tests de structures de condition :~%")

(run-test-case mv 1 '(if (> 1 0) 1 0) "IF")

(run-test-case mv 200 '(cond ((= 2 3) 5) ((> 1 100) 9) ((< 1 2) 200)) "COND")

(run-test-case mv 1 '(if (and (< 0 2) (>= 3 3)) 1 0) "AND")

(run-test-case mv 1 '(if (or (< 0 2) (>= 3 3)) 1 0) "OR")
