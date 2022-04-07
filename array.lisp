(defvar *stack-size*)
(defvar *top*)
(defvar *stack*)

;;初期化
(defun init-stack (n)
  (setq *stack-size* n
	*top* 0
	*stack* (make-array n)))

;;データをスタックに詰む
(defun push-down (data)
  (when (< *top* *stack-size*)
    (setf (aref *stack* *top*) data)
    (incf *top*)))
;;スタックからデータを取り出す
(defun pop-up ()
  (when (plusp *top*)
    (decf *top*)
    (aref *stack* *top*)))

;;素数のチェック
(defun primep (n ps)
  (dotimes (i (length ps) t)
    (let ((m (aref ps i)))
      (cond
	((zerop (mod n m)) (return))
	((< n (* m m )) (return t))))))

;;素数のベクタを返す
(defun prime (n)
  (do ((ps (make-array 1 :fill-pointer t :adjustable t
			 :initial-element 2))
       (m 3 (+ m 2)))
      ((< n m) ps)
    (if (primep m ps)
	(vector-push-extend m ps))))
(defun rmake (n)
  (do ((rslt (make-array n :fill-pointer 0 :adjustable t))
       (i 0 (1+ i)))
      ((< n i) rslt)
;;    (print rslt)
    (vector-push (+ 130 (* 0.1 (random 349))) rslt)))
       
			
(defun hei (vec)
  (format t "  階級|　　度数　　累積度数")
  (print    "-------------------------------")
  (terpri)
  (labels ((make-count (inf sup data)
	     (do ((i 0 (1+ i))
		  (rslt 0))
		 ((> (1+ i) (length data)) rslt)
	       (if (and (< (aref data i) sup)
			(<= inf (aref data i)))
		   (incf rslt)))))
    (let ((counts 0))
      (do ((inf 130 (+ 5 inf)) (accum 0))
	  ((>= inf 165) accum)
	(setq counts (make-count inf (+ 5 inf) vec))
	(incf accum counts)
      (format t "~d - ~d | ~d ~d~%" inf (+ 5 inf) counts accum)))))
		  
(defun ave-var (vec)
  (let ((len (length vec)) (ave 0) (vari 0))
    (do ((i 0 (1+ i)))
	((> (1+ i) len))
      (incf ave (aref vec i)))
      (setf ave (/ ave len))
    (format t "average is ~d~%" ave)
    (do ((i 0 (1+ i))(delta 0) )
	((> (1+ i) len))
      (setf delta (- (aref vec i) ave))
      (incf vari (* delta delta)))
    (setf vari (/ vari len))
    (setf vari (sqrt vari))
    (format t "variance is ~d~%" vari)))

(defun maximum (xs) ;;xs is vector
  (let ((len (length xs)))
    (do ((i 0 (1+ i)) (rslt (aref xs 0)))
	((> (1+ i) len) rslt)
      (if (< rslt (aref xs i))
	  (setf rslt (aref xs i))))))

(defun minimum (xs)
  (let ((len (length xs)))
    (do ((i 0 (1+ i)) (rslt (aref xs 0) ))
	((> (1+ i) len) rslt)
      (if (> rslt (aref xs i))
	  (setf rslt (aref xs i))))))

(defun fibo-iter (n)
  (do ((a 0 b)
       (b 1 (+ a b))
       (i 0 (1+ i)))
      ((= i n) a)))

(defun pascal (n)
  (let ((buff (make-array (1+ n) :initial-element 0)))
    (setf (aref buff 1) 1)
    (dotimes (i n)
      (do ((j (1+ i) (1- j)))
	  ((zerop j))
	(format t "  ~3D" (setf (aref buff j)
				(+ (aref buff j) (aref buff (1- j))))))
      (terpri))))

(defun fibo2 (n)
  (if (< n 2)
      n
      (+ (fibo2 (- n 1)) (fibo2 (- n 2)))))

(defvar *fib-table*
  (make-array 2 :fill-pointer t :adjustable t :initial-contents '(0 1)))

(defun fibo-sub (n)
  (do ((i (length *fib-table*) (1+ i)))
      ((> i n) (aref *fib-table* n))
    (vector-push-extend (+ (aref *fib-table* (1- i))
			   (aref *fib-table* (- i 2)))
			*fib-table*)))
(defun fibo (n)
  (if (< n (length *fib-table*))
      (aref *fib-table* n)
     (fibo-sub n)))
