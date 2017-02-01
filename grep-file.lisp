;;;; File manipulation (10 points)                                                


;;; Code

;; This method writes the lines in file1 that contain str to file2; it is case   
;; sensitive if the optional fourth argument equal to TRUE is passed             
(defun grep-file (file1 str file2 &optional case-sen &aux temp (num 0)
                        (infile (open file1 :direction :input
                                      :if-does-not-exist :error))
                        (outfile (open file2 :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)))
  (loop
   (setf temp (read-line infile nil 'end-of-file))
   (when (eq temp 'end-of-file) (close outfile) (close infile) (return))
   (if (eq case-sen t)
       ;; case-sensitive case                                                    
       (if (eq (search str temp) nil) ()
         (prog1 (format t "Line ~d: ~a~&" (incf num) temp)
           (format outfile "~a~&" temp)))
     ;; non-case-sensitive case                                                  
     (if (eq (search (string-downcase str) (string-downcase temp)) nil) ()
       (prog1 (format t "Line ~d: ~a~&" (incf num) temp)
         (format outfile "~a~&" temp))))))


;;; Interaction

#|

test.txt-

yep
this is the second line in the file.
This is the third line in the file..
What!? Where are you going.

This is the last line in the file.


[411]> (grep-file "test.txt" "this" "testout.txt")
Line 1: this is the second line in the file.
Line 2: This is the third line in the file..
Line 3: This is the last line in the file.
NIL
[412]> (grep-file "test.txt" "this" "testout.txt" t)
Line 1: this is the second line in the file.
NIL


testout.txt-

this is the second line in the file.

|#