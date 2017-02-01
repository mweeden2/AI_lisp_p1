;;;; Symbolic Differentiation (20 points)


;;; Code

;; This function returns the symbolic derivative of the passed expression
(defun deriv (expn var)
  (cond ((not (listp expn))             ; turn exp into a list if it is not one
	 (deriv (list expn) var))
        ((eq (second expn) nil)         ; constant case
         (if (eq (first expn) var)
             1 0))
        ((eq (first expn) '+)           ; sumation case
	 `(+ ,(deriv (second expn) var) ,(deriv (third expn) var)))
        ((eq (first expn) '-)           ; diference case
	 `(- ,(deriv (second expn) var) ,(deriv (third expn) var)))
        ((eq (first expn) '*)           ; multiplication case
	 `(+ (* ,(second expn) ,(deriv (third expn) var))
             (* ,(third expn) ,(deriv (second expn) var))))
        ((eq (first expn) '/)           ; division case
         `(/ (- (* ,(third expn) ,(deriv (second expn) var))
                 (*,(second expn) ,(deriv (third expn) var))
                 (expt ,(third expn) 2))))
        ((eq (first expn) 'exp)         ; exponential case
         `(* (exp ,(second expn)) ,(deriv (second expn) var)))))


;;; Interaction

#|

[316]> (deriv '(x) 'x)
1
[317]> (deriv '(y) 'x)
0
[318]> (deriv '(22) 'x)
0
[319]> (deriv '(+ 1 x) 'x)
(+ 0 1)
[320]> (deriv '(- y x) 'x)
(- 0 1)
[321]> (deriv '(* 2 y) 'y)
(+ (* 2 1) (* Y 0))
[322]> (deriv '(/ 2 y) 'y)
(/ (- (* Y 0) (* 2 1) (EXPT Y 2)))
[323]> (deriv '(* 2 (exp x)) 'x)
(+ (* 2 (* (EXP X) 1)) (* (EXP X) 0))
[324]> (deriv '(/ x (- y 1)) 'x)
(/ (- (* (- Y 1) 1) (* X (- 0 0)) (EXPT (- Y 1) 2)))

|#