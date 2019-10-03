;Karimov Danil 2 group, 2 course, PMI Lesson 2

;task 1 //list
'(red green blue) ;quote
(list 'red 'green 'blue) ;list
(cons 'red (cons 'green (cons 'blue nil))) ;cons

;task 2 //list in list
(list (list 'blue 'sky) (list 'green 'grass) (list 'brown 'earth)) ;list realization
(cons (cons 'blue (cons 'sky nil))  (cons (cons 'green (cons 'grass nil)) (cons (cons 'brown (cons 'earth nil))nil))) ;cons realization

;task 3 //Say Hello 
(defun greet (l) (cons 'Hello l))
(greet '(Neo)) ;Follow the white rabbit
(greet '(Danil Karimov)) ;result greet

;task 4 //(Defenition) Good people
(defun def (x y)
	(list x 'is 'a y))
(def 'Liza 'God) ;Liza is a GOD

;task 5 //long defenition
(defun long-def (x y)
	(append x '(is) '(a) y))
(long-def '(Liza) '(Good girl)) ;result="Liza is a good girl"

;task 6 //length list
(length '(a '(b c) d)) ;result 3

;task 7 //equal list
(equal '(a '(b c) d) '(a b '(c d))) ;result NIL
(equal '(a '(b c) d) '(a '(b c) d)) ;result T

;task 8 //car\cdr
(setq cards '((q spades) (7 hearts) (6 clubs) (5 diamonds) (2 diamonds)))
(defun my-second (l) (car (cdr l))) ;second elem list

;task 9 //c*a*d*r (4)
(defun my-fourth (l) (cadddr l)) ;return fourth elem list
(my-fourth cards) ;result (5 diamonds)

;task 10 //c*a*d*r (6)
(defun my-six (l) (car (cdr (cddddr l)))) ;return nil
(my-six cards) ;return NIL

;task 11 //car\cdr and NULL list
(car nil) ;return nil
(cdr nil) ;return nil

;task 12 //change firts  word to elem
(defun Ch_F_W (l x) 
	(list x (cdr l))) ;Change first elem 
(CH_F_w cards '(first)) ;new list with changes

;task for "IF/COND"

;task 13 //odd number
(defun say-oddness (x)
	(if (evenp x) 'even 'odd)) ;EVEN or not EVEN
(say-oddness 2) ;say-even
(say-oddness 3) ;say-odd

;task 14 //abs(x)
(defun my-abs (x)
	(if (< x 0) (* x -1) x)) ;abs(x)
(my-abs -1) ;return 1
(my-abs 2) ;return 2

;task 15 //make number(+ 1) and return number if result even, or return result(+1) 
(defun make-even (x) 
	(if (evenp (+ x 1)) (+ x 1) x))
(make-even 2) ;return 2
(make-even 3) ;return 4

;task 16 //result of "if" (short calculate)
(defun R_if (x)
	(if (= x 0) (r_if x) x)) ;zerop with "if"
(r_if 2) ;result 2
(r_if 0) ;resul "stack overflow"

;task 17 //new "NOT"
(defun my-not (x)
	(if (equal x nil) T nil)) 
(my-not nil) ;result T
(my-not 2) ;result nil

;task 18 //new "AND"
(defun my-and (x y)
	(if (equal x T) y x))
(my-and T Nil) ;result Nil
(my-and Nil T) ;result Nil
(my-and Nil Nil) ;result Nil
(my-and T T) ;Result T

;task 19 //New "sign"
(defun my-sign-1 (x)
	(if (> x 0) 1  (if (= x 0) x -1)))
(defun my-sign-2 (x)
	(cond 
	((> x 0) 1)
	((< x 0) -1)
	((= x 0) 0)))
(my-sign-1 2) ;result 1
(my-sign-1 -3) ;result -1
(my-sign-1 0) ;result 0
(my-sign-2 2) ;result 2
(my-sign-2 -3) ;result -3
(my-sign-2 0) ;result 0

;task 20 //World Capitals
(defun Where-is (x)
	(cond
	((equal x 'Moscow) 'Russia)
	((equal x 'Brasilia) 'Brazil)
	((equal x 'Canberra) 'Australia)
	(T 'unknown))) 
(Where-is 'Moscow) ;Russia
(Where-is 'Brasilia)	;Brazil
(where-is 'Canberra)	;Australia
(where-is 'Roma)	;Italy, but we don't know this

;task 21-22 //What list? (Great/Awful)
(defun emphasis (l)
	(cond
	((equal (car l) 'Good) (cons 'great (cdr l)))
	((equal (car l) 'Bad) (cons 'awful (cdr l)))
	(T l)))
(emphasis '(good l1 l2)) ;Great list
(emphasis '(bad l3 l4)) ;Awful list
(emphasis '(unknown l5 l6)) ;Unknown list

;task 22 //emphasis v2.0
;I did it before
(emphasis '(realy? why?))

;task 23 //Return T if 0<x<100 and x odd
(defun small-positive-oddp (x)
 	(and (if (> x 0) (if (< x 100) T  nil)  nil)
	     (if (equal (say-oddness x) 'odd) T nil)))
(small-positive-oddp 2) ;result Nil
(small-positive-oddp 1) ;result T
(small-positive-oddp -1) ;result Nil
(small-positive-oddp 101) ;result Nil

;task 24 //Return T if A>B or A or B = 0
(defun GTEST (a b)
	(if (or (= a 0) (= b 0) ) T (if (> a b) T nil)))
(gtest 1 2) ;result Nil
(gtest 2 1) ;result T
(gtest 0 1) ;result T
(gtest -1 0) ;result T

;task 25 //Short calculate (sc) "and" and "or"
(and nil 1) ;result Nil (sc)
(or T 123) ;result T (sc)

;task 26 //sc with words
(or 'George nil 'Harry) ;result George

;task 27 //How-alike (how alike 2 number)
(defun how-alike (x y)
     (cond
     ((= x y) 'THE-SAME)
     ((and (equal (say-oddness x) 'odd) (equal (say-oddness y) 'odd) 'BOTH-ODD))
     ((and (equal (say-oddness x) 'even) (equal (say-oddness y) 'even) 'BOTH-EVEN))
     ((and (< x 0) (< y 0)) 'both-negative)
     (T 'not-alike))))
(how-alike 1 1) ;the-same
(how-alike 4 2) ;both-even
(how-alike 1 3) ;both-odd
(how-alike -1 -2) ;botth-negative
(how-alike 1 2) ;not-alike


