;Karimov Danil 2 group, 2 course, PMI
;Homework 3

;task 1 //I did it.

;task 2 //N!!=N*(N-2)*(N-4)....
(defun Dfact1 (n)
 (cond 
 ((evenp n) (if (= n 2) 2 (* n (dfact1 (- n 2) ) ) ))
 ((not (evenp n)) (if (= n 1) 1 (* n (dfact1 (- n 2))))) ))) ;normal rec.

(dfact1 5) ;result 15
(dfact1 4) ;result 8

(defun Dfact2 (f n)
 (if (or (= n 2) (= n 1)) f (dfact2 (* f n) (- n 2)))) ;tail rec.
(dfact2 1 5) ;result 15
(dfact2 2 4) ;result 8

;task 3 //max in list
(defun max-list1 (l)
 (cond 
 ((null l) nil) ;if is empty-nil
 ((null (cdr l)) (car l)) ;if have 1-it is max
 ((> (car l) (max-list1 (cdr l))) (car l))  ;rec. in this line. choose max
 (t (max-list1 (cdr l))) ;if nothing-go next element
 )
) ;normal rec. 

(max-list1 '(1 3 2))   ;result 3

(defun max-list2h (f l)
 (cond
 ((null l) f) ;if is empty-f//nill
 ((null (cdr l)) (car l)) ;if have 1-it is max
 ((> (car l) (max-list2h f (cdr l))) (car l)) ;tail rec.
 (t (max-list2h f (cdr l))) ;if norhing-go next tail rec.
 )
) ;tail rec.

(defun max-list2 (l)
 (max-list2h nil l)) ;call tail rec.
(max-list2 '(1 2 3)) ;result 3

;task 4 //Intersperce of list
(defun Intersperce1 (l k)
 (cond
 ((null l) k)
 (T (cons (car l) (Intersperce1 k (cdr l)))))) ;normal rec.
(Intersperce1 '(A B C) '(1 2 3 4 5)) ;result (A 1 B 2 C 3 4 5)

(defun mixh (l k)
 (cond 
 ((mull l) k)
 ((null k) l)
 ((Intersperce2 l k))))

(defun Intersperce2 (l k)
 (if l (cons (car l) (Intersperce2 k (cdr l))))) 
(Intersperce2 '(1 2 3 4 5) '(A B C D)) ;result (1 A 2 B 3 C 4 D 5)


