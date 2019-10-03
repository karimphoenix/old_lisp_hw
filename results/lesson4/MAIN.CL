;Karimov Danil 2 group, 2 course, PMI
;Lesson 4

;task 1 //Square-all: (2 3 4)->(4 9 16)
(defun square (l)
 (* l l)) ;help function for square-all
(square 2) ;result 4 

;function SQUARE-ALL
(defun Square-all (l)
 (mapcar #'square l))

(square-all '(2 3 4)) ;result (4 9 16)

;task 2 //((one un) (two deux))->(one two)
(setf WORDS '((one un) (two deux) (three trois) (four quatre) (five cinq)))

;function FIRST-ALL
(defun First-all (l)
 (mapcar #'first l))

(first-all words) ;result (one two three four five), use standart function "first"

;task 3 //plus-7: (1 2 3)->(8 9 10)
;function plus-7 with lambda-function
(defun plus-7 (l)
 (mapcar #'(lambda (x) (+ x 7)) l))

(plus-7 '(1 2 3)) ;result (8 9 10)

;task 4 //plus-n:((x y z) n)->((x+n) (y+n) (z+n))
(defun plus-n (l n)
 (mapcar #'(lambda (x) (+ x n)) l))
(plus-n '(1 2 3) 4) ;equal plus-4:->result (5 6 7)

;task 5 //find-odd: find first number > 3
(defun find-odd (l)
 (find-if #'(lambda (x) (> x 3)) l)) ;use find-if
(find-odd '(2 3 4 5)) ;result 4
(find-odd '(2 2 1)) ;result nil

;task 6 //find-proper-devident: ((2 3 4) 3)) -> 3
(defun find-proper-devident (l n)
 (find-if #'(lambda (x) (= (mod x n) 0)) l)) ;use find-if
(find-proper-devident '(1 4 3) 2.0) ;result 4
(find-proper-devident '(1 2 3) 6.0) ;result nil

;task 7 //count-zeros:(1 0 2 3)-> 1
(defun count-zeros (l) ;use REMOVE-IF-NOT
 (length (remove-if-not #'(lambda (n) (zerop n)) l))) ;length list wthiout zeros 

(count-zeros '(1 0 2 3)) ;result 0
(count-zeros '(1 2 3)) ;result 0
(count-zeros '(0 0 0)) ;result 3

;task 8 //sum all list: (1 2 3)->6
(defun my-sum (l)  ;use REDUCE
 (reduce '+ l)) ;sum all list element
(my-sum '(1 2 3)) ;result 6
(my-sum '(0 0 0)) ;result 0
(my-sum '()) ;result 0

;task 9 //concat-all: (concat-all WORDS)->(one un two deux...)
(defun concat-all (l)
 (reduce #'append l)) ;use REDUCE and APPEND
(concat-all WORDS) ;result (ONE UN TWO DEUX THREE TROIS FOUR QUATRE FIVE CINQ)

;task 10 //Inalienable-rights: use function for list:
; (life liberty and the pursuit of happiness)
(defun inalienable-rights (f)
 (funcall f '(life liberty and the pirsuit of happiness))) ;use FUNCALL
(inalienable-rights 'length) ;result 7
(inalienable-rights 'reverse) ;result (HAPPINESS OF PIRSUIT THE AND LIBERTY LIFE) 
(inalienable-rights 'car) ;result LIFE
(inalienable-rights 'cdr) ;result (LIBERTY AND THE PIRSUIT OF HAPPINESS)

;end of task.



