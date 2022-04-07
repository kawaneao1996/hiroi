(dotimes (x 5) (print x))

(defun fact (x)
  (let ((result 1))
    (dotimes (n x result)
      (setq result (* result (1+ n))))))

(defun my-count (x ys)
  (let ((c 0))
    (dolist (y ys c)
      (if (eql x y) (incf c)))))

(defun fact (x)
  (let ((result 1) (i 2))
    (loop
      (if (> i x) (return result))
    (setq result (* result i)
	  i (1+ i)))))

(defun midpoint (n)
  (let ((w (/ 1d0 n)) (s 0d0))
    (dotimes (i n (* s w))
      (let ((x (* (+ i 0.5d0) w)))
	(incf s (/ 4d0 (+ 1d0 (* x x))))))))

(defun primep (n ps)
  (dolist (m ps t)
    (if (zerop (mod n m)) (return))))

(defun prime (n)
  (do ((ps '(2)) (m 3 (+ m 2)))
      ((< n m) ps)
    (if (primep m ps)
	(setq ps (append ps (list m))))))

(defun fast-primep (n ps)
  (dolist (m ps t)
    (cond
      ((zerop (mod n m)) (return))
      ((< n (* m m)) (return t)))))

(defun fast-prime (n)
  (do ((ps '(2)) (m 3 (+ m 2)))
      ((< n m) ps)
    (if (fast-primep m ps)
	(setq ps (append ps (list m))))))

(defun 9-9 ()
  (format t "   | 1 2 3 4 5 6 7 8 9~%")
  (format t "---+------------------~%")
  (dotimes (i 9)
    (format t "~2D | " (1+ i))
    (dotimes (j 9)
      (format t "~2D " (* (1+ i) (1+ j))))
    (terpri)))
(defun my-find (x ys)
  (if (null ys) nil
      (if (equal x (car ys)) (car ys)
	  (my-find x (cdr ys)))))
(defun my-find2 (x ys)
  (dolist (y ys)
    (if (eql x y) (return y))))

(defun product (1st 2nd)
  (if (> 1st 2nd) nil
      (let ((rslt 1))
	(dotimes (n (- 2nd 1st) rslt)
	  (setq rslt (* (+ 1st n 1) rslt))))))
(defun longerp (xs ys)
  (do ((xsl xs (cdr xsl))
       (ysl ys (cdr ysl)))
      ((or (null xsl) (null ysl)) xsl)))

(defun comb (n r)
  (if (or (= r n) (= r 0)) 1
      (/ (* (comb n (1- r)) (1+ (- n r))) r)))

(defun pascal (n)
  (dotimes (i n)
    (dotimes (j (1+ i))
      (format t "~3D " (comb i j)))
    (terpri)))

(defun writelist (lst)
  (dolist (x lst nil)
    (format t "~3D " x)))


;(defun pascal2 (n)
 ; (dotimes (i n nil)
  ;  (

(defun makepas (lst dan)
  (let ((plst (append '(0) lst '(0))) (rslt nil))
    (dotimes (i  dan rslt)
;      (writelist plst)
;      (terpri)
      (setq rslt (cons (+ (nth i plst) (nth (1+ i) plst)) rslt))
      )))
	  
(defun pascal2 (n)
  (let ((pas '(1)))
    (dotimes (i n)
      (writelist pas)
      (terpri)
      (setq pas (makepas pas (+ i 2))))))
(defparameter *stack* nil)

(defun push-stack (data)
  (setq *stack*
	(cons data *stack*)))

(defun pop-stack ()
  (prog1
      (car *stack*)
    (setq *stack*
	  (cdr *stack*))))
(defun my-make-list (n x)
  (let ((rslt nil))
    (dotimes (i n)
      (setq rslt
	    (cons x rslt)))
    rslt))

(defun my-reverse (lst)
  (do ((i  0 (1+ i) )(rslt nil))
      ((> i (length lst)) rslt)
;    (print i)
    (setq rslt (cons (pop lst) rslt))))
    
(defun my-revappend (xs ys)
  (let ((rslt ys))
    (dolist (x xs)
      (setq rslt (cons x rslt))
      )
    rslt))

(defun rpn (lst)
  (let ((stack nil) (slst lst))
    (dolist (x slst stack)
      (cond
	((numberp x)  (push (pop slst) stack)
	 (princ stack))
	((functionp (function x))
	 (push (funcall (quote x) (pop slst) (pop slst))
	       stack))
	(t (princ "nonsuitable"))))))

(defun rpn2 (lst)
  (let ((stack nil) (slst lst))
    (dolist (x slst stack)
      (cond
	((numberp x)  (push (pop slst) stack)
	 (princ stack))
	(t
	 (eval `(,x  ,stack))

	 )))))

(let ((x '+) (s '(1 1)))
  (eval `(,x  ,(quote s))))

(defun rpn3 (lst)
  (let ((stack nil) (slst lst))
    (dolist (x slst stack)
      (cond
	((numberp x) (push (pop slst) stack))
	(t (eval (append  (list (quote x)) stack)))))))

(defun rpn (xs)
  (let ((zs nil))
    (dolist (x xs (if (and (consp zs) (null (cdr zs)))
			   (car zs)
			   "invalid expression"))
      (if (numberp x)
	  (push x zs)
	  (let ((b (pop zs)) (a (pop zs)))
	    (if (or (null b) (null a))
		(return "stack underflow"))
	    (case
		x
	      (+ (push (+ a b) zs))
	      (- (push (- a b) zs))
	      (* (push (* a b) zs))
	      (/ (push (/ a b) zs))
	      (t (return "invalid operarion"))))))))
