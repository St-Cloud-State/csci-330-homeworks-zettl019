;This is the partion function it will take the unsorted list and partition it into two  

(defun partition-list (unsorted-list)
  (cond
    ((eql unsorted-list nil)
     (list nil nil))                                ; Empty list
    ((eql (cdr unsorted-list) nil)
     (list unsorted-list nil))                      ; Only one element
    (t
     (let ((rest (partition-list (cddr unsorted-list)))) ; Recursively partition the rest
       (list (cons (car unsorted-list) (car rest))     ; First half
             (cons (cadr unsorted-list) (cadr rest))))))) ; Second half

;This is the merge function that merges the two sorted lists into one single sorted list
; It compares the first elements of both lists and appends the smaller one to the result

(defun merge-sorted (sorted1 sorted2)
  (cond
    ((eql sorted1 nil) sorted2)
    ((eql sorted2 nil) sorted1)
    ((< (car sorted1) (car sorted2))
     (cons (car sorted1) (merge-sorted (cdr sorted1) sorted2)))
    (t
     (cons (car sorted2) (merge-sorted sorted1 (cdr sorted2))))))

;This is the merge sort function which will merge the two sorted halves together
(defun merge-sort (unsorted-list)
  (cond
    ((eql unsorted-list nil) unsorted-list)
    ((eql (cdr unsorted-list) nil) unsorted-list)
    (t
     (let ((halves (partition-list unsorted-list)))
       (merge-sorted (merge-sort (car halves))
                     (merge-sort (cadr halves)))))))

;This is the list that will be used in the script file 
 (merge-sort '(3 1 4 1 5 9 2 6 5))
