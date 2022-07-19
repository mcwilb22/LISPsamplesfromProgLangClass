; Brandi McWilliams
; C311 Homework 6
; Due March 3, 2021

; Ex 1a Next-day function:
; Using a complex conditional to test to see if the paramater equals a given day of the week.  Then returning next day of the week.

(defun next-day (day)
  (cond
   ((equal day 'Monday)(setq day 'Tuesday)day)
   ((equal day 'Tuesday)(setq day 'Wednesday)day)
   ((equal day 'Wednesday)(setq day 'Thursday)day)
   ((equal day 'Thursday)(setq day 'Friday)day)
   ((equal day 'Friday)(setq day 'Saturday)day)
   ((equal day 'Saturday)(setq day 'Sunday)day)
   ((equal day 'Sunday)(setq day 'Monday)day)))
next-day
(next-day 'Tuesday)
Wednesday

(next-day 'Friday)
Saturday

(next-day 'Thursday)
Friday


;Ex 1b. Random day generator.
; I set a list using the days of the week.  I then implemented the random number fucntion to select a number 0-6.  I utilized elt to select the day of the week and stored that into randomdayis.  I utilized a second elt to establish the next day in the list.  Lastly, I used several princ functions to print the information out.  I used the symbol-name to retrieve the string and capitalize the first letter but using the built-in lisp function capitalize.  

(setq daysoftheweek '(monday tuesday wednesday thursday friday saturday sunday))
(monday tuesday wednesday thursday friday saturday sunday)

(defun randomday (daysoftheweek)
  (setq randomnum (% (random) 6))
  (setq randomdayis (elt daysoftheweek randomnum))
  (setq tomorrowis (elt daysoftheweek (+ randomnum 1)))
    (princ "Today is: ")
    (princ (capitalize (symbol-name randomdayis)))
    (princ "\n")
    (princ "Tomorrow is: ")
    (princ (capitalize (symbol-name tomorrowis)))
    (princ "\n")t)
randomday

(randomday daysoftheweek)
Today is: Monday
Tomorrow is: Tuesday
t

(randomday daysoftheweek)
Today is: Saturday
Tomorrow is: Sunday
t

(randomday daysoftheweek)
Today is: Wednesday
Tomorrow is: Thursday
t

(randomday daysoftheweek)
Today is: Thursday
Tomorrow is: Friday
t

(randomday daysoftheweek)
Today is: Tuesday
Tomorrow is: Wednesday
t


; Ex 2 First-fit
; I implemented dotimes to iterate through the list.  I stored the length of the list into 'lengthofthelist' so as not to call the length functionf or every iteration of the loop.  I check to see if the list is empy and return nil if it is.  Then the loop compares the car of the list to the number passed in.  If the car is < the number then pop the element from the list.  Else, set firstfit to the car of the list and return it. 
(defun first-fit (list number)
  (let (firstfit))
  (setq lengthofthelist (length list))
  (cond
   ((not list) (setq firstfit nil)))
  (dotimes (count lengthofthelist firstfit)
    (if (< (car list) number)
	(pop list)
      (setq firstfit (car list)))))
first-fit

(first-fit '(3 8 6 4 9) 5)
8
(first-fit '() 9)
nil
(first-fit '(27 37 43 52 89) 47)
52
(first-fit '(13) 13)
13



; Ex 2 Best-fit
; I chose to implement do times and some if statements to determine the bestfit for the element number that is passed into the function.  I first used let to set bestfit to nil.  I also set up lengthofthelist to contain the list length using the function length.  Doing this means I am not calling the function for every iteration of dotimes.  (Thanks for that tip from the last homework!) I used a conditional to test if the list was empty and return nil.  Then I used dotimes to traverse the list.  The first if statement compares the car of the list to the number passed in.  If the car is < the number, I used pop to remove the first element of the list.  For the second if statement I compare the car of the list to the cadr of the list to see if it is larger.  If the car of the list is larger then I know to set the best value as the cadr of the list.  Then pop the element from the list.  The last if statement then compares the bestvalue that is store with that of the number originally passed into the function.  If the best value is >= number then it is stored as the best fit and returned. 

(defun best-fit (list number)
  (let (bestfit))
  (setq lengthofthelist (length list))
  (cond
   ((not list) (setq firstfit nil)))
  (dotimes (count lengthofthelist bestfit)
    (if (< (car list) number)
	(pop list))
    (if (>= (car list) (cadr list))
	(setq bestvalue (cadr list))
      (pop list))
    (if (>= bestvalue number)
	(setq bestfit bestvalue))))
best-fit


(best-fit '(3 8 6 4 9) 5) ; Example in homework.
6
(best-fit '(27 40 43 52 39 89) 38) ; Testing some other numbers.
39
(best-fit '(27 40 43 52 89 39) 38) ; Testing further in the list for comparisons.
39
(best-fit '(27 40 43 52 89 38) 38) ; Just testing, equality, to see if bestfit = bestvalue that it returns the same number 38 = 38 in this instance.
38

