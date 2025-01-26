
(format t "~%Tests de fonctions Fibo et Fact :~%")

(run-test-fonction mv 3628800 '(fact 10) '(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))) "Factorielle")

(run-test-fonction mv 55 '(fibo 10) '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))) "Fibonacci")
