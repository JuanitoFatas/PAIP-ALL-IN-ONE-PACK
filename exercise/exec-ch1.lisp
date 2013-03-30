;;;; Exercise of PAIP, Chapter 1

;;; 1.1 Define a version of last-name that handles "Rex Morgan MD," "Morton Downey, Jr.," and whatever other cases you can think of.

(defparameter *suffixes*
  '(Jr Jr. Junior Sr Sr. Senior MD I II II III IV V VI VII VIII IX X XI XII XIII XIV XV XVI XVII XVIII XIX XX M.A. J.D. M.D. D.O. D.C. Ph.D. M.B.A. B.A.)
  "A list of suffixes will be ignored.")

(defun last-element (lst)
  (first (last lst)))

(defun last-name (name)
  "Returns the last name from a given name (must be a list)."
  (last-element (remove-if #'(lambda (n)
                               (member n *suffixes*)) name)))

;;; 1.2 Write a function to exponentiate, or raise a number to an integer power. For example: (power 3 2) => 9

(defun power1 (x n)
  "find power nth of x"
  (expt x n))

(defun power2 (x n)
  "find power nth of x, n must be an integer > 0.
   executes in n time."
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(defun power3 (x n)
  "find power nth of x, n must be an integer > 0.
   executes in log n time, because of the check for even n."
  (cond 
   ((= n 0) 1)
   ((evenp n) (expt (power x (/ n 2)) 2))
   (t (* x (power x (- n 1))))))

;;; 1.3 Write a function that counts the number of atoms in an expression. E.g.: (count-atoms '(a (b) c)) => 3. Notice that there is something of ambiguity in this: should (a nil c) count as three atoms, or as two, because it is equivalent to (a () c)?

(defun count-atoms1 (exp)
  (length exp))

(defun count-atoms (exp)
  "Returns the total number of non-nil atoms in the expression"  
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(defun count-all-atoms (exp &optional (if-null 1))
  "Returns the total number of non-nil atoms in the expression
   counting nil as an atom only in non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))

;;; 1.4 Write a function that counts the number of times an expression occurs anywhere within another expression. Example: (count-anywhere 'a '(a ((a) b) a)) => 3

(defun count-anywhere1 (e exp)
  (cond ((null exp) 0)
        ((eql e exp) 1)
        ((atom (car exp))
         (if (eq e (car exp))
             (+ 1 (count-anywhere1 e (cdr exp)))
             (count-anywhere1 e (cdr exp))))
        ((listp (car exp))
         (+ (count-anywhere1 e (car exp))
            (count-anywhere1 e (cdr exp))))))

(defun count-anywhere2 (item tree)
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere2 item (first tree))
              (count-anywhere2 item (rest tree))))))

;;; 1.5 Write a function to compute the dot product of two sequences of numbers, represented as lists. The dot product is computed by multiplying corresponding elements and then adding up the resulting products. E.g.: (dot-product '(10 20) '(3 4)) = 10x3 + 20x4 = 110

(defun dot-product1 (s1 s2)
  (if (or (null s1) (null s2))
      0
      (+ (* (first s1) (first s2))
         (dot-product1 (rest s1) (rest s2)))))

(defun dot-product2 (s1 s2)
  (let ((sum 0))
    (dotimes (i (length s1))
      (incf sum (* (elt s1 i) (elt s2 i))))
    sum))

(defun dot-product3 (s1 s2)
  (apply #'+ (mapcar #'* s1 s2)))

