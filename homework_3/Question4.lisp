;This is the function that inserts an item into a sorted list in ascending order
;It keeps track of already examined elements using the checked-list
;If the sorted-list is empty then the item is simply added to the front
;If the item is greater than the last element the last element is removed then insertion is continued

(defun inserting (sorted-list checked-list item)
  (cond ((eql sorted-list nil)
         (append (list item) checked-list)) ; If sorted-list is empty, place item at the front
        ((> item (car (last sorted-list)))
         (inserting (butlast sorted-list)
                    (append (last sorted-list) checked-list)
                    item)) ; If item is larger then remove the last element and try inserting again
        (t
         (append sorted-list (list item) checked-list)))) ; Insert item into the correct position

;This is the function that performs one pass of insertion
;It takes the first element from the unsorted list and inserts it into the sorted list
;The checked list starts as empty since nothing has been examined yet

(defun one-pass (sorted-list unsorted-list)
  (inserting sorted-list nil (car unsorted-list)))

;This is the function that continues making passes until the entire list is sorted
;Each pass removes one element from the unsorted list and inserts it into the sorted list
;When the unsorted list is empty, the sorting process is complete

(defun make-passes (sorted-list unsorted-list)
  (cond ((eql unsorted-list nil) sorted-list) ; If unsorted list is empty, return the sorted list
        (t (make-passes (one-pass sorted-list unsorted-list)
                        (cdr unsorted-list))))) 

;This is the insertion sort function that starts the sorting process
;The sorted list is initially empty, and the unsorted list is the original input list
;It calls make-passes to complete the sorting

(defun insertion-sort (unsorted-list)
  (make-passes nil unsorted-list))

;This is the example list that will be used in the script file
(insertion-sort '(5 2 9 1 5 6))
