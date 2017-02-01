;;;; Data as program (7 points)


;;; Code

;; This functions evaluates a procedure provided by the user on a random number between
;; 0.0 and 1.0
(defun data-to-program (&aux temp rand)
  (write-string (format nil "Enter a one-line function definition: ~&"))
  (eval (setf temp (read)))
  (setf rand (random 1.0))
  (write-string (format nil "Applying your function ~a to ~a returns ~a."
			(second temp) rand (eval `(,(second temp) ,rand))))
  nil)


;;; Interaction

#|

[498]> (data-to-program)
Enter a one-line function definition: 
(defun two-pwr (x) (expt 2 x))
Applying your function TWO-PWR to 0.74742126 returns 1.6787894.
NIL
[499]> (data-to-program)
Enter a one-line function definition: 
(defun percent (x) (* x 100))
Applying your function PERCENT to 0.51371115 returns 51.371117.
NIL
[500]> (data-to-program)
Enter a one-line function definition: 
(defun coin-flip (x) (< x 0.5))
Applying your function COIN-FLIP to 0.563722 returns NIL.
NIL

|#