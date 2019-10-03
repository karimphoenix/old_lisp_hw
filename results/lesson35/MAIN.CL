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
 (one-lvlh (cdr l) f))0)) ;result-numbers of not first lvl list.

(defun one-lvl (l)
 (if (zerop (one-lvlh l 0)) T nil)) ;use tail rec. with option.
(one-lvl '(1 2 3)) ;result-T->one-lvl list
(one-lvl '((1) 2 3)) ;result-nil->not one-lvl list
(one-lvl '()) ;one-lvl list.

;task 4 //first atom in list.
(defun first-atom (l)
 (cond 
 ((null l) nil)
 ((atom (car l)) (car l))
 (t (first-atom (cdr l))))) ;rec. function-result->first atom in list
(first-atom '(1 2 3)) ;result 1
(first-atom '((1) 2 3)) ;resutl 2

;task 5 //delete first occurrence in list.
(defun dfo (l a) 
 (cond 
 ((null l) nil)
 ((equal (car l) a) (cdr l))
 (T (cons (car l) (dfo (cdr l) a)))))
(dfo '(1 2 3) 1) ;result (2 3)
(dfo '((1) 2 3) 1) ;result ((1) 2 3)

;task 6 //delete elem whith even numbers
(defun Delete-even-Num (l)
 (if l (cons (car l) (delete-even-num (cddr l)))))
(delete-even-num '(1 2 3 4 5)) ;result (1 3 5)

;task 7 //(a b c d...)->((a b) (c d)...)
(defun devpair (l)
 (cond
 ((null l) nil)
 ((null (cdr l)) nil) 
 (T (cons (list (car l) (cadr l)) (devpair (cddr l))))))
(devpair '(a b c d)) ;result ((a b) (c d))
(devpair '((1) 2 (3 4) 5)) ;result (((1) 2) ((3 4) 5))

;task 8 // (a b...)+(1 2 ...)->(a 1 b 2...) repeat-task 4 from Homework 3
(defun Intersperce (l k)
 (cond
 ((null l) k)
 (T (cons (car l) (Intersperce k (cdr l)))))) ;normal rec.
(Intersperce '(A B C) '(1 2 3 4 5)) ;result (A 1 B 2 C 3 4 5)  

;task 9 //return first-equal occurrence in two list.

(defun helpf (x l)
(cond
((null l) Nil)
(t (If (equal (car l) x) (Car l) (helpf x (Cdr l)))))) ;help function (tail rec.)

(defun FIRST-EQUAL (x y)
(cond
((Null x) Nil) ;if list is empty or don't have equal-nil
((equal (Car x) (helpf (Car x) y)) (Car x))
(t (FIRST-EQUAL (Cdr x) y)) )) ;function use tail rec.

;Note.
;Evaluate (compile) individually select functions.
;ie first we select the first-compile (evaluate), and then select a second-evaluate. Then ;apply to the responses.

(FIRST-EQUAL '(a d g f e ) '(c d b e)) ;result d

