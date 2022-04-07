(defun  add-element (xs ys)
  (if (or (null xs) (null ys))
      nil
      (cons (+ (car xs) (car ys))
	    (add-element (cdr xs) (cdr ys)))))

(mapcar #'+ '(1 2 3 4) '(10 20 30 40))

(apply #'+ '(1 2 3))

(apply #'car '((a b c)))

(apply #'+ 4 5 6 '(1 2 3))

(defun cube (x y z)
  (* x y z))

(mapcar #'length '("abc" "def" "hijkl" "mnopqr"))

(apply #'+ *)

(defun execfunc (func arg1 arg2)
  (funcall func arg1 arg2))

(member '(a d) '((a d) (a b) (a c) (a e)) :test #'equal)

(defun map1 (fn xs)
  (if (null xs)
      nil
      (cons (funcall fn (car xs))
	    (map1 fn (cdr xs)))))
(defun map2 (fn xs ys)
  (if (or (null xs) (null ys))
      nil
      (cons (funcall fn (car xs) (car ys))
	    (map2 fn (cdr xs) (cdr ys)))))

(map1 (lambda (x) (* x x)) '(1 3 5 7 9))

(map2 (function (lambda (x y) (* x y))) '(1 3 5 7 9) '(2 4 6 8 10))

(defun filter (pred xs)
  (cond ((null xs) nil)
	((funcall pred (car xs))
	 (cons (car xs) (filter pred (cdr xs))))
	(t (filter pred (cdr xs)))))
	 
(defun fold-left (fn a xs)
  (if  (null xs)
       a
       (fold-left fn (funcall fn a (car xs)) (cdr xs))))

(defun fold-right (fn a xs)
  (if (null xs)
      a
      (funcall fn (car xs) (fold-right fn a (cdr xs)))))
(fold-left #'+ 0 '(1 2 3 4 5))
(fold-right #'+ 0 '(1 2 3 4 5))

(defun sum-of (fn e s)
  (cond
    ((< e s) (princ "e < s"))
    (t
     (do ((i s (1+ i)) (rslt 0))
	 ((> i e) rslt)
       (setq rslt (+ rslt  (funcall fn i)))
       ))))

(defun tabulate (fn e s)
  (cond
    ((< e s) nil)
    (t
     (do ((i s (1+ i)) (rslt nil))
	 ((> i e) rslt)
       (push (funcall fn i) rslt)))))

(defun  foreach (fn xs)
  (dolist (x xs)
    (funcall fn x)))
    
(defun my-maplist (fn xs)
  (apply fn xs))

(defun my-count-if (pred xs)
  (let ((rslt 0))
    (dolist (x xs rslt)
      (if (funcall pred x)
	  (incf rslt)))))

(defun my-find-if (pred xs)
  (if (funcall pred (car xs)) (car xs)
      (my-find-if pred (cdr xs))))

(defun scan-left (fn a xs)
  (let ((stack nil))
    (if (null xs)
	(progn
	  (push a stack)
	  stack)
	(push (funcall fn
		       (funcall fn a (car xs))
		       (cdr xs))
	      stack)
	)))

(defun scan-left (fn a xs)
  (if (null xs)
      (list a)
      (cons a (scan-left fn (funcall fn (car xs) a)
			 (cdr xs)))))

(defun scan-right  (fn a xs)
  (if (null xs)
      (list a)
      (let ((ys (scan-right fn a (cdr xs))))
	(cons (funcall fn (car xs) (car ys)) ys))))

(defun my-reverse (xs)
  (if (null xs)
      nil
      (append (my-reverse (cdr xs)) (list (car xs)))))

(defun my-rev-sub (xs zs)
  (if (null xs)
      zs
      (my-rev-sub (cdr xs) (cons (car xs) zs))))

(defun my-reverse2 (xs)
  (my-rev-sub xs (my-rev-sub xs nil)))

(defun my-reverse3 (xs &optional zs)
  (if (null xs)
      zs
      (my-reverse3 (cdr xs) (cons (car xs) zs))))

(defun foo (a b &rest x) (print a ) (print b) (print x))

(defun bar (&rest z) (print z))

(defun sum-of-rec (fn s e &optional (a 0))
  (if (> s e)
      a
      (sum-of-rec fn (1+ s) e (+ a (funcall fn s)))))

(defun sum-of-loop (fn s e &aux (a 0))
  (dotimes (i (1+ (- e s)) a)
    (print i)
    (incf a (funcall fn (+ s i)))))

(defun my-count-if-rec (pred xs &optional (c 0))
  (if (null xs)
      c
      (my-count-if-rec pred (cdr xs) (if (funcall pred (car xs)) (1+ c) c))))

(defun my-count-if-loop (pred xs &aux (c 0))
  (dolist (x xs c)
    (if (funcall pred x)
	(incf c)
	)))

(defun my-member (x xs &key (test #'eql))
  (if (or (null xs) (funcall test x (car xs)))
      xs
      (my-member x (cdr xs) :test test)))

(defun map1 (fn xs)
  (if  (null xs)
       nil
       (cons (funcall fn (car xs))
	     (map1 fn (cdr xs)))))

(defun mapn (fn &rest args)
  (if (member nil args)
      nil
      (cons (apply fn (map1 #'car args))
	    (apply #'mapn fn (map1 #'cdr args)))))

;;(defun mymapn (fn &rest args)
;;  (funcall mapcar 
