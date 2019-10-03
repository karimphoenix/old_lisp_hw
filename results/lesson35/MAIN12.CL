;Karimov Danil 2 group, 2 course, PMI
;Lesson 3.5-recursion

;task 1 //Last elem of list.
(last '(1 2 3)) ;check result standert function //result (3)
(last '((1) 2 (3 4 5))) ;//result ((3 4 5))
(last '(1)) ;//result (1)
(last '()) ;//result nil

(defun my-last (l)  ;my function "last"
 (cond 
 ((null l) nil) ;if is empty-nil
 ((null (cdr l)) (cons (car l) nil)) ;if have 1 elevent-it's last element(result-list)
 (T (my-last (cdr l))) ;if not last-go next elem
 )
)
(my-last '(1 2 3)) ;check my function "last" //result (3)
(my-last '((1) 2 (3 4 5))) ;result ((3 4 5))
(my-last '(1)) ;result (1)
(my-last '()) ;result nil

;task 2 //Delete last list element
(defun delete-last-v1 (l) ;first version function delete last element
 (cond
 ((null (cdr l)) nil) ;if 1 elem-nil
 (T (cons (car l) (delete-last (cdr l)))) ;construct new list.
 )
)

(delete-last-v1 '(1 2 3))
(delete-last-v1 '(1))
(delete-last-v1 '(1 2 (3 4)))

(defun delete-last-v2 (l) ;second version function-not recursion function.
 (reverse (cdr (reverse l))))) ;this is easier =)
(delete-last-v2 '(1 2 3))

;task 3 //one-lvl list.
(defun one-lvlh (l f)
 (if l
 (+ (if (atom (car l)) (+ 0 f) (+ f 1))
 (one-lvlh (cdr l) f))0))

(defun one-lvl (l)
 (if (zerop (one-lvlh l 0)) T nil))
(one-lvl '(1 2 3)) ;result-T->one-lvl list
(one-lvl '((1) 2 3)) ;result-nil->not one-lvl list