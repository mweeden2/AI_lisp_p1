;;;; N-Queens (18 points)


;;; Code

;; This function returns the index of the minimum element in a list
(defun index-of-min (l leng)
  (if (null (second l))
      (1- leng)
    (if (eq (first l) (apply #'min l))
	(- leng (length l))
      (index-of-min (rest l) leng))))

;; This function returns a list of the number of conflicts for one possible queen
;; placement
(defun num-conflicts (n queens row col)
  (if (eq (length queens) (1+ col)) ; if the queen is in the column being evaluated
      (if (eq (length queens) 1)
	  0
	(num-conflicts n (butlast queens) row col))
    (if (eq (length queens) 1) ; base case
	;; if the queens are on the same row
	(if (or (eq row (first (first (last queens))))
		;; if the queen is diagonal of the one in question
		(eq (abs (- row (first (first (last queens)))))
		    (abs (- col (first (last (first (last queens))))))))
	    1 0)
      ;; if the queens are on the same row
      (if (or (eq row (first (first (last queens))))
	      ;; if the queen is diagonal of the one in quenstion
	      (eq (abs (- row (first (first (last queens)))))
		  (abs (- col (first (last (first (last queens))))))))
	  (+ 1 (num-conflicts n (butlast queens) row col))
	(num-conflicts n (butlast queens) row col)))))

;; This function returns a list of conflicts for each row in a given col
(defun get-col-conflicts (n queens num-rows col)
  (if (eq num-rows 0)       ; base case      
      (list (num-conflicts n queens num-rows col))
    (append (get-col-conflicts n queens (1- num-rows) col)
	    (list (num-conflicts n queens num-rows col)))))

;; This function adds a queen to a list of queens (list of row and col values) using
;; the min-conflicts heuristic
(defun add-queen (n queens)
  (append queens
	  ;; select the row with min conflicts
	  (list (list 
		 (index-of-min (get-col-conflicts n queens (1- n) (length queens)) n)
		 (length queens)))))	; col number of new queen

;; This function completes one iteration of re-evaluating conflicted queens
(defun iterate-queen-check (n queens &optional (col (1- n)))
  (if (eq col 0)
      (substitute (list (index-of-min (get-col-conflicts n queens (1- n) 0) n) col)
		  (first queens)
		  queens)
    (iterate-queen-check n
			 (substitute 
				  (list (index-of-min (get-col-conflicts
						       n queens (1- n) col) n) col)
				  (nth col queens)
				  queens)
			 (1- col))))

;; This funntion makes a list of the numbers of conflicts for all positions
(defun all-conflicts (n queens &optional (col (1- n)))
  (if (eq col 0)			;base case
      (list (num-conflicts n queens (first (nth col queens)) 0))
    (append (all-conflicts n queens (1- col))
	    (list (num-conflicts n queens (first (nth col queens)) col)))))

;; This function prints the column and row values of n queens that are not in
;; in conflict with each other on a nxn board
(defun n-queens (n max-steps &optional (queens '((0 0))))
  (if (< (length queens) n)
      (n-queens n max-steps (add-queen n queens))
    (if (eq max-steps 0)
	(format nil "~&This program was unable to find a solution.")
      (if (eq 0
	      (apply #'max (all-conflicts n queens)))
	  (format nil "~&Queens: ~a" queens)
	(n-queens n (1- max-steps) 
		  ;; if iteration is stuck in a local min...
		  (if (equal (iterate-queen-check n queens) queens)
		      (iterate-queen-check 
		       ;; ...manually change the second queen.
		       n (substitute 
			  (append (list (random n)) (second queens))
			  (second queens)
			  queens))
		    (iterate-queen-check n queens)))))))
					 
			   
					  

(clc)

;;; Interaction

#|

[1,470]> (n-queens 4 2)
"Queens: ((2 0) (0 1) (3 2) (1 3))"
[1,471]> (n-queens 5 2)
"Queens: ((0 0) (2 1) (4 2) (1 3) (3 4))"
[1,472]> (n-queens 6 2)
"This program was unable to find a solution."
[1,473]> (n-queens 6 5)
"Queens: ((3 0) (0 1) (4 2) (1 3) (5 4) (2 5))"
[1,474]> (n-queens 10 8)
"Queens: ((8 0) (3 1) (5 2) (7 3) (1 4) (6 5) (0 6) (2 7) (4 8) (9 9))"

|#