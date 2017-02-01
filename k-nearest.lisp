;;;; A learning program! (20 points)                                                                             


;;; Code (this function is not operational)

;; This function ("nearest-neighbor") learns how to classify instances with two, real-valued attributes         
;; using the k nearest neighbor method.                                                                         

(defun read-train-file (f1 &aux temp (infile (open f1 :direction :input :if-does-not-exist :error)))
" Function to read in training data"

  (loop
   (let ((next (read infile nil 'end-of-file)))
     (when (eq next 'end-of-file)
           (close infile)
           (return temp))
     (push (list next
                 (read infile nil 'end-of-file)
                 (read infile nil 'end-of-file)
                 (read infile nil 'end-of-file))
           temp))))

(defun read-test-file (f1 &aux temp (infile (open f1 :direction :input :if-does-not-exist :error)))
" Function to read in test data"
  (loop
   (let ((next (read infile nil 'end-of-file)))
     (when (eq next 'end-of-file)
           (close infile)
           (return temp))
     (push (list next
                 (read infile nil 'end-of-file)
                 (read infile nil 'end-of-file))
           temp))))

(defun find-neighbors (k new stored &optional result)
  (progn (if (null result) 
	     (setf result (make-list k :initial-element (make-list 5 :initial-element most-positive-fixnum))))
;	 (print result)
	 (cond ((not (<= (length stored) 1))
		;; calculate and save the distance between points
		(progn (setf (first stored) 
			     (append (first stored) `(,(+ (abs (- (second (first stored)) (first new)))
							  (abs (- (third (first stored)) (second new)))))))	
		       (loop for i from 0 to (1- k) do
			     (progn ; (print (format nil "k: ~a i: ~a result: ~a~&(nth i result): ~a" k i result (nth i result)))
				    ;; if this stored point's distance is less than the distance of the ith result point
				    (if (< (first (last (first stored))) (fifth (nth i result)))
					;; then insert the stored point as a result in that result's place
					(return (setf result 
						      (append (subseq result 0 i)
							      (list (first stored))
							      (subseq result (1+ i) k)))))))
		       (find-neighbors k new (rest stored) result)))
	       ;; base case
	       (t
		;; calculate and save the distance between points
		(progn (setf (first stored)
			     (append (first stored) `(,(+ (abs (- (second (first stored)) (first new)))
							  (abs (- (third (first stored)) (second new)))))))
		       (loop for i from 0 to (1- k) do
			     ;; if this stored point's distance is less than the distance of the ith result point
			     (if (< (fifth (first stored)) (fifth (nth i result)))
				 ;; then pass the stored point as a result in that result's place
				 (return (setf result 
					       (append (subseq result 0 i)
						       (list (first stored))
						       (subseq result (1+ i) k))))))
		       result)))))


(clc)

(defun nearest-neighbor ()
"Driver function for the learning program"

  (format t "Input training file name: ")
  (let ((train-set (read-train-file (read))))
    (format t "Read ~d training instances~&~%" (length train-set))
    (format t "Input test file name: ")
    (let ((test-set (read-test-file (read))))
      (format t "Read ~d test instances~&~%" (length test-set))
      (format t "~&Input value of k for calculating k-nearest-neighbors: ")
      (let ((k (read)))
        (dolist (triplet test-set (format t "~&~%Isn't this cool?~&"))
                (let ((neighbors (find-neighbors k triplet train-set)))
                  (format t "~3&*************************************~& test point: ~a~&" triplet)
		  (setf class 0)
		  (loop for point in neighbors do 
			(progn (subseq point 0 5)			      
			       (if (eq (first point) 0)
				   (decf class)
				 (incf class))))
		  (format t "~2&=== The likely class of ~a is ~a, since, of the ~a nearest neighbors, ~a are of class ~3:*~a.~&"
			  triplet
			  (if (minusp class) 0 1)
			  k
			  ;; count how many nearest neighbors are of the majority class	  
			  (if (minusp class)
				(- (/ (- class (length neighbors)) 2))
			      (/ (+ class (length neighbors)) 2)))
		  (loop for point in neighbors do
			(if (= (first point) (if (minusp class) 0 1))
			       (print (subseq point 0 5))))
		  (if (or (and (minusp class) ; print message for opposing neighbors if any
			       (> class (- k))) (and (>= class 0) (< class (length neighbors))))
		      (prog1 (format t "~2&=== The ~a other nearest neighbor~:P ~[is~;are~] of class ~a.~&" 
				     (if (minusp class) ; argument for the number of remaining neighbors
					 (/ (+ class (length neighbors)) 2)
				       (- (/ (- class (length neighbors)) 2)))
				     (if (minusp class) ; argument to decide to print "is" or "are"
					 (- (/ (+ class (length neighbors)) 2) 1)
				       (- (- (/ (- class (length neighbors)) 2)) 1))
				     (if (minusp class) 1 0))
			(loop for point in neighbors do
                        (if (= (first point) (if (minusp class) 1 0))
                               (print (subseq point 0 5))))))
				
		  
                  ;; You have to write code here to complete the program
			  ))))))


;;; Interaction (This function is not operational)

#|



|#