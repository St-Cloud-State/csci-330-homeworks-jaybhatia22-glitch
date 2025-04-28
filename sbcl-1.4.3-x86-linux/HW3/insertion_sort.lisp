(defun insert (item sorted)
  (if (or (null sorted) (< item (car sorted)))
      (cons item sorted)
      (cons (car sorted) (insert item (cdr sorted)))))

(defun insertion-sort (lst)
  (if (null lst) '()
      (insert (car lst) (insertion-sort (cdr lst)))))
