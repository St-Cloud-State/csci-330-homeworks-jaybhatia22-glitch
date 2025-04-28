;; Step 1: Split the list into two halves
(defun split-list (lst)
  (if (<= (length lst) 1)
      (values lst nil)  ;; Return the list as is if it has 0 or 1 element
      (let* ((middle (floor (length lst) 2))
             (left-part (subseq lst 0 middle))
             (right-part (subseq lst middle)))
        (values left-part right-part))))

;; Step 2: Combine two sorted lists into one sorted list
(defun merge-sorted (list1 list2)
  "Merges two already sorted lists into one sorted list."
  (cond
    ((null list1) list2)          ;; If the first list is empty, return the second list
    ((null list2) list1)          ;; If the second list is empty, return the first list
    ((< (car list1) (car list2))   ;; Compare the first elements of both lists
     (cons (car list1) (merge-sorted (cdr list1) list2)))
    (t
     (cons (car list2) (merge-sorted list1 (cdr list2))))))

;; Step 3: Recursive Mergesort function
(defun sort-list (lst)
  "Sorts a list using the Mergesort algorithm."
  (if (or (null lst) (null (cdr lst))) 
      lst  ;; Base case: If the list has 0 or 1 element, it's already sorted
      (multiple-value-bind (left right) (split-list lst)  ;; Split the list
        (merge-sorted (sort-list left) (sort-list right)))) ;; Merge the sorted halves

