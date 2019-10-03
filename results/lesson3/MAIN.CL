;Karimov Danil 2 group, 2 course, PMI
;Lesson 3

;task 1 //n! forward and tail recursion
(defun fact1 (x)
  (if (zerop x) 1 (* x (fact1 (- x 1)))))
(fact1 5) ;return 120

(defun fact2-impl (f n)
 (cond 
 ((zerop n) f)
 (t (fact2-impl (* n f) (- n 1))))) ;using in next fuction
(defun fact2 (n)
 (fact2-impl 1 n)) ;use "help function" written above
(fact2 5) ;result 120

;task 2 //recurent sum
(defun sum1 (a b)
 (if (= b a) a (+ b (sum1 a (- b 1))))) ;forward recursion
(sum1 2 4) ;result 9

(defun sumThelp (f a b)
 (if (= b a) f
 (sumTHelp (+ f b) a (- b 1))))
(defun sumAB_V2_0 (a b)
 (sumTHelp a a b))
(sumAB_v2_0  2 4)

;task 3 //count list elem.
(defun legth_v1 (l)
 (if (not l) 0 (+ 1 (legth_v1 (cdr l))))) ;count list elements
(legth_v1 '(1 2 3)) ;result 3


(defun legth_help_f (l f)
 (if (not l) f (legth_help_f (cdr l) (+ 1 f)) )) ;tail rec.
(defun legth_v2 (l)
 (legth_help_f l 0))) ;tail rec.

(legth_v2 '(1 2 3)) ;result 3

;task 4 //list have odd numbers
(defun anyoddph (l f)
 (cond
 ( (not l) f) 
 ( (not (evenp (car l)) ) (+ f 1) )
 (T (anyoddph (cdr l) 0) )))))  ;result 1-i.e. 1-true, 0-nil

(defun anyoddp (l)
 (anyoddph l 0)) 
(anyoddp '(1 2 3)) ;result 1-true
(anyoddp '(2 2 2)) ;result 0-nil

;task 5 //summ all numbers in list
(defun add-up1 (l)
 (if (not l) 0 (+ (car l) (add-up1 (cdr l)) ))) ;normal recursion
(add-up1 '(1 2 3)) ;result 6

(defun add-up2 (f l)
 (if (not l) f (add-up2 (+ f (car l)) (cdr l))))) ;tail recursion
(add-up2 0 '(1 2 3)) ;result 6

;task 6 //humor function
(defun laugh1 (n)
 (if (zerop n) nil (cons 'xa (laugh1 (- n 1))) )) ;normal recursion
(laugh1 4) ;result (XA XA XA XA)

(defun laugh2 (f n)
 (if (zerop n) f (laugh2 (cons 'xa f) (- n 1)))) ;tail rec.
(laugh2 nil 4) ;result (XA XA XA XA)

;task 7 //comparison list 
(defun compare-list (l k)
 (cond
 ((= (legth_v2 l) (legth_v2 k)) 'List-are-equal)
 ((> (legth_v2 l) (legth_v2 k)) 'First-is-longer )
 ((< (legth_v2 l)  (legth_v2 k)) 'Second-is-longer))) ;tail rec. use in legth_v2
(compare-list '(1 2 3) '(2 3 4)) ;equal
(compare-list '(1 2 3) '(2 3)) ;first>second
(compare-list '(1 2) '(2 3 4)) ;first<second

;task 8 //fib-normal rec.
(defun fib (n)
 (if (or (zerop n) (= n 1)) 1 (+ (fib (- n 2)) (fib (- n 1))))) 
(fib 4) ;result 5-fib sequences f0=1 f1=1 f2=2 f3=3 f4=5 and etc.

;task 9 //fib-tail rec.
(defun fibT (n f)
 (if (or (zerop n) (= n 1)) f (+ (fibT (- n 2) (+ f (- n 1))))))
(fibt 4 1) ;result 5-fib sequences f0=1 f1=1 f2=2 f3=3 f4=5 and etc.



