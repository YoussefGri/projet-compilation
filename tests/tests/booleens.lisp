(format t "~%Tests d'opérateurs booléens :~%")

(run-test-case-cond mv T '(= 2 2) "= ")
(run-test-case-cond mv T '(> 2 1) "> ")
(run-test-case-cond mv T '(< 2 3) "< ")
(run-test-case-cond mv T '(>= 2 2) ">=")
(run-test-case-cond mv T '(>= 3 2) ">=")
(run-test-case-cond mv T '(<= 2 2) "<=")
(run-test-case-cond mv T '(<= 2 3) "<=")
