;Karimov Danil 2 group, 2 course, PMI
;Lesson 4
;Additional tasks

;task 1 //to implement the right convolution: "right reduce"
;like (reverse (reduce (reverse l)))
(defun increase (f l)
 (cond
 ((null l) )
 ((null (cdr l)) (car l))
 (t (funcall f (car l) (increase f (cdr l))))))) ;use funcall and rec.

(increase #'+ '(1 2 3)) ;result 6 //reduce: result 6
(increase #'equal '(1 2 3)) ;result nil //reduce: result nil
