; Brandi McWilliams
; Homework 5 C311
; Due February 24, 2021

; Ex 1 While loop and GCD 
; I implemented a while loop with a LCV comparing the remainder value to 0.  The loop executes until the remainder reaches 0.  The loop then breaks and returns the divdend value.  I initialized remainder to equal the divisor at the beginning, after a few mishaps where I had tried setting remainder to null and that would give me an error during the comparison operation of 0 and null.  
(defun gcd (divisor dividend)
  (let ((remainder divisor))
    (while (/= 0 remainder)
      (setq remainder (mod dividend divisor))
      (setq dividend divisor)
      (setq divisor remainder)))
  dividend)

gcd
(gcd 12 18)
6
(gcd 33 64)
1
(gcd 100 15)
5
(gcd 168 180)
12
(gcd 220 1323)
1


; Ex 2.
; 2a. Lambda Expression - Replace function.
; I set up the function replace using a lambda function.  The function tests the parameter x and compares to parameter z.  If they are equal, then the function replaces z with y.  This function is then applied using mapcar to iterate through a list and replace all occurences of a number z with the value of y.  

(defun replace (L z y)
  (mapcar
   (lambda (x)
     (if (equal x z)
	 (setq x y)
       x))
   L))
replace

(replace '(3 1 5 6 3 2 3) 3 9)
(9 1 5 6 9 2 9)

(replace '(1 3 2 3 4 3 5 3 6 3 7 3 8) 3 17)
(1 17 2 17 4 17 5 17 6 17 7 17 ...)

; 2b. Print-List function.
; I implemented a lambda function within print-list to print a list with spaces between each element of the list. My first efforts, I left out the &rest.  This worked okay for the function by itself, but when I attempted to use the function with funcall or apply, I would receive error messages that too many arguments were being passed into the print-list function.  Therefore, I applyed the &rest which allowed both funcall and apply to work successfully.  

(defun print-list (&rest list)
  (mapc
   (lambda (x)
     (princ x)
     (princ " "))
   list))
print-list
(print-list '(1 2 3 4 5))
(1 2 3 4 5) ((1 2 3 4 5))

(print-list '(125 275 350 500))
(125 275 350 500) ((125 275 350 500))

(funcall 'print-list 22 44 66 88 100)
22 44 66 88 100 (22 44 66 88 100)

(apply 'print-list '(125 250 375 500))
125 250 375 500 (125 250 375 500


; Ex 3 List Manipulation
; Ex 3a. Make Multiples: I implemented the dotimes loop to create a list that creates multiples of n.  The loop ends when x reaches m.  I used the push function to store the multiples in a list.  Then I utilized the reverse function I created in homework 3 to reverse the list using dolist to iterate through the resultlist and applying cons to reverse the list.  

(defun make-multiples (n m)
  (let ((result 0)(resultlist)(reversedlist))
  (dotimes (x m resultlist)
    (setq result (+ result n))
    (setq resultlist (push result resultlist)))
  (dolist (y resultlist reversedlist)
  (setq reversedlist (cons y reversedlist)))))
make-multiples
(make-multiples 3 4)
(3 6 9 12)

(make-multiples 5 7)
(5 10 15 20 25 30 35)

(make-multiples 9 9)
(9 18 27 36 45 54 63 72 81)


; Ex 3b. Is Multiple:  I set up the function using dotimes.  The loop executes until the end list L.  It works if the list is multiples in ascending order.  I've included output examples using lists from the make-multiples function, in 3a, above as well.  As of now I cannot get the function return true for lists that are not in ascending order.   


(defun is-multiple (L)
  (let ((result)(first (car L)))
  (dotimes (count (length L) result)
    (setq result (= 0 (mod (cadr L) first))))))
is-multiple
(is-multiple '( 3 6 9 12))
t
(is-multiple '(2 4 6 8 10 12 14))
t
(is-multiple '(12 4 3 2))
nil


(setq B (make-multiples 9 9))
(9 18 27 36 45 54 63 72 81)

(is-multiple B)
t

(setq C (make-multiples 6 8))
(6 12 18 24 30 36 42 48)

(is-multiple C)
t
