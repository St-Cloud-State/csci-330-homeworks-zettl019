;This is the function that will partition the list into sorted pairs
;It takes the original list and groups adjacent elements into pairs
;If there is an odd number of elements the last element will remain in a single list
(defun make-pairs (original-list)
  (cond ((eql original-list nil) '()) ; If the list is empty, return an empty list
        ((eql (cdr original-list) nil)
         (list (list (car original-list)))) ; If only one element is left, put it in a single list
        (t
         (if (< (car original-list) (cadr original-list))
             (cons (list (car original-list) (cadr original-list))
                   (make-pairs (cddr original-list))) ; Recursively process the rest of the list
             (cons (list (cadr original-list) (car original-list))
                   (make-pairs (cddr original-list))))))) ; Swap elements if needed

;This is the merge function that merges two sorted lists into a single sorted list
;It compares the first elements of both lists and appends the smaller one to the result
(defun merge-sorted (list1 list2)
  (cond ((eql list1 nil) list2) ; If the first list is empty, return the second list
        ((eql list2 nil) list1) ; If the second list is empty, return the first list
        ((< (car list1) (car list2))
         (cons (car list1) (merge-sorted (cdr list1) list2))) ; Take from list1 if smaller
        (t
         (cons (car list2) (merge-sorted list1 (cdr list2)))))) ; Otherwise, take from list2

;This is the function that merges adjacent lists in one pass
;It takes a list of sorted lists and merges them two at a time
;If there is an odd number of lists the last one remains unmerged for that pass
(defun one-pass (list-of-lists)
  (cond ((eql list-of-lists nil) list-of-lists) ; If the list is empty, return it
        ((eql (cdr list-of-lists) nil) list-of-lists) ; If only one list remains, return it
        (t (cons (merge-sorted (car list-of-lists) (cadr list-of-lists))
                 (one-pass (cddr list-of-lists)))))) ; Merge pairs recursively

;This is the function that repeatedly makes passes until the list is fully merged
;It keeps calling one-pass until only one sorted list remains
(defun make-passes (list-of-lists)
  (if (eql (length list-of-lists) 1)
      (car list-of-lists) ; If only one list remains, return it
      (make-passes (one-pass list-of-lists)))) ; Otherwise, keep merging

;This is the bottom-up merge sort function that sorts the entire list
;First it partitions the list into sorted pairs using make-pairs
;Then it repeatedly merges adjacent lists using make-passes until fully sorted
(defun bottom-up-merge-sort (original-list)
  (make-passes (make-pairs original-list)))

;This is the example list that will be used in the script file
(bottom-up-merge-sort '(1 7 2 1 8 6 5 3))
