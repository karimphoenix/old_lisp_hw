;Karimov Danil 2 group, 2 course, PMI
;Homework 2

;task 1-I did all

;task 2 //C*A*D*R
(caar '((blue cube) (red piramid))) ;result caar

;task 3 //CAAR
(caar '(fred nil)) ;Error:Incorrect list structure (..) for CAAR Aborting.

;task 4 //with cons a,b,c,d->((a b) (c d))
(defun cons4 (a b c d)
  (cons (cons (car a) (cons (car b) nil)) (cons (cons (car c) (cons (car d) nil))nil)))
(cons4 '(a) '(b) '(c) '(d))


;task 5 //(deeper 'z)->(z)
(defun deeper (l)
  (list l))
(deeper 'z) ;result (z)

;task 6 //a b->(a b), and a<b
(defun ordered (x y)
  (cond 
  ((if (> x y) (list y x) (list x y)))
  (T (list x y)))))
(ordered 1 2) ;result (1 2)
(ordered 2 1) ;result (1 2)
(ordered 2 2) ;result (2 2)

;task 7 //(fun x y)-> fun=sum-of->x+y, fun=product-of->x*y, else (x y)
(defun compute (fun x y)
  (if (equal fun 'sum-of) (+ x y) (if (equal fun 'product-of) (* x y) (list x y))))
(compute 'sum-of 2 3) ;result 5
(compute 'product-of 2 3) ;result 6
(compute 'list  2 3) ;result (2 3)

;task 8 //same-sign(x y): if (x<0, y<0) or (x>0, y>0) or (x=0, y=0)->T 
(defun same-sign (x y)
  (if (or (or (and (< x 0) (< y 0)) 
          (and (> x 0) (> y 0))
          (and (= x 0) (= 0 y)))) T nil)))
(same-sign 2 3) ;return T
(same-sign -1 -2) ;return T
(same-sign 0 0) ;return T
(same-sign -1 2) ;return NIL

