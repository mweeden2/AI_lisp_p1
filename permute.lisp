;;;; Permutation


;;; Code

;; This function return all posible permutations of the passed list
(defun permute (l)
  (cond ((null l)
         '())
        ((null (cdr l))
         (list l))
	(t
         (loop
          for element in l
          append (mapcar (lambda(x) (cons element x))
                  (p (remove element l :count 1)))))))


;;;; Interaction

#|

[186]> (permute '(1 2 3)) 
((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
[187]> (permute '(a b c d))
((A B C D) (A B D C) (A C B D) (A C D B) (A D B C)
 (A D C B) (B A C D) (B A D C) (B C A D) (B C D A)
 (B D A C) (B D C A) (C A B D) (C A D B) (C B A D)
 (C B D A) (C D A B) (C D B A) (D A B C) (D A C B)
 (D B A C) (D B C A) (D C A B) (D C B A))

|#