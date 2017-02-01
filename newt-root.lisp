;;;; finding the fourth root of a number by Newton's method (10 points)


;;; Code

;; This function returns the fourth root of x using the Newton-root method
(defun newt-root (x &optional tol x0)
  (cond ((not (numberp tol))            ; define tol as 0.1 if not given
         (newt-root x 0.1 x0))
        ((not (numberp x0))             ; define x0 if not defined
	 (newt-root x tol (float (/ x 4))))
        ((< (abs (- (expt x0 4) x)) tol)
         x0)
        (t
         (newt-root x tol (- x0 (float (/ (- (expt x0 4) x) (* 4 (expt x0 3)))))))))


;;; Interaction

#|

[292]> (newt-root 16 0.1)
2.000041
[293]> (newt-root 16 1)
2.0074282
[294]> (newt-root 16 0.001)
2.0
[295]> (newt-root 64 0.1)
2.8284304

|#
