
(defun leafy-delete-at (list index)
  "Delete the element at INDEX in LIST and return the modified list."
  (if (= index 0)
      (cdr list)
    (let ((previous (nthcdr (1- index) list)))
      (setcdr previous (cdr (cdr previous)))
      list)))

(defun leafy-remove-at (list index)
  "Return a new list with the element at INDEX removed."
  (unless (listp list)
    (user-error "leafy-remove-at: Not a list - list:<%S>" list))
  (append (cl-subseq list 0 index)
          (cl-subseq list (1+ index))))

(defun leafy-join (sequence separator)
  ;; Filter null or empty
  (let ((filtered (seq-filter (lambda (s) (and s (not (string-empty-p s)))) sequence)))
  (mapconcat #'identity filtered separator)))

(defun alist-set (key value alist)
  "Set the value associated with KEY in ALIST to VALUE.
If KEY is already present, update its value; otherwise, add a new key-value pair to ALIST."
  (let ((pair (assoc key alist)))
    (if pair
        (setcdr pair value)
      (push (cons key value) alist))
    alist))

(provide 'leafy--utils)
