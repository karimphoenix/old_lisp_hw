;Karimov Danil 2 group, 2 course, PMI
;Homework 4

;task 1 //I did it! =)

;task 2 //reverse list in list:((one un) (two deux))->((un one) (deux two))
(setf WORDS '((one un) (two deux) (three trois) (four quatre) (five cinq)))
(defun rlil (l)
 (mapcar #'reverse l)) ;use MAPCAR->REVERSE list in list
(rlil WORDS) ;resutl ((UN ONE) (DEUX TWO) (TROIS THREE) (QUATRE FOUR) (CINQ FIVE))

;task 3 //sum 2 list:(1 2 3), (2 3 4)->(3 5 7)
(defun sum2list (l1 l2)
 (mapcar #'+ l1 l2)) ;use MAPCAR->+ (car l1) (car l2), etc.
(sum2list '(1 2 3) '(2 3 4)) ;result (3 5 7)
(sum2list '(0 0 0) '(0 0 0)) ;result (0 0 0)
(sum2list '() '()) ;result nil

;task 4 //paste in list of list:((one un) (two deux)...) (uno dos...)->
;->((one un uno) (two deux dos)...) etc.
(defun PasteInLoL (l1 l2)
 (mapcar #'(lambda (x y) (append x (list y))) l1 l2)) ;use MAPCAR and LAMBDA:
; ((a) (b)) (c d)->((a c) (b d))

(PasteInLoL Words '(uno dos tres quatro cinco)) ;result ((ONE UN UNO) (TWO DEUX DOS) (THREE TROIS TRES) (FOUR QUATRE QUATRO) (FIVE CINQ CINCO)

;task 5 //Greater-than-N:returns predicate "result>N"
(defun make-greater-than-predicate (n)
 #'(lambda (x) (> x n)))
(funcall (make-greater-than-predicate 8) 9) ;result T
(funcall (make-greater-than-predicate 8) 7) ;result Nil
 
;task 6 //remove element from list if it <= N: (1 2 3) 2->(3) 
(defun Remove-from-list-smN (l n)
 (remove-if-not (make-greater-than-predicate n) l))
(remove-from-list-smN '(1 2 3) 2) ;result (3)
(remove-from-list-smN '(1 2 3 4 5 6 7) 3) ;result (4 5 6 7)

;task 7 //every element in list-atom: (1 2 3)->T (1 (2) 3)->Nil
(defun all-list-atom (l)
 (every #'atom l))
(all-list-atom '(1 2 3)) ;result T
(all-list-atom '(1 (2) 3)) ;result Nil

;task 8 //sum length include list: (1 (2) 3)->1 (1 2 3)->0
(defun length-LiL (l)
 (reduce #'+ (mapcar #'Length (remove-if #'atom l))))
(length-LiL '((1) (2) (3))) ;result 3
(length-LiL '(1 2 3)) ;result 0
(length-LiL '(((1) (2) 3))) ;result 3

;task 9 //sum lengt include list, that less N:(1 (2 3 4) 5) 2->0
(defun length-LiL-lessN (l n)
 (reduce #'+ (remove-if #'(lambda (x) (> x n)) (mapcar #'length (remove-if #'atom l)))))
(length-LiL-LessN '((1) (2 3 4) (5)) 2)))  ;result 2
(length-LiL-LessN '((1) (2) (3)) 1)) ;result 3
(length-LiL-LessN '((1) (2 3) (4) (5)) 2) ;result 5
(length-LiL-lessN '((1 (2 3) 4 (5))) 2) ;result 0

