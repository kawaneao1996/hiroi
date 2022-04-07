(defun comb2 (n r)
  (if (or (= r 0) (= r n)) 1
      (/ (* (comb2 n (1- r)) (1+ (- n r))) r)))

(defun comb3 (n r)
  (if (or (= r 0) (= r n)) 1
      (+ (comb3 n (1- r)) (comb3 (1- n) r))))

(defun fact (n)
  (let ((c 1) (rslt 1))
    (loop
      (if (> c n) (return rslt))
      (setq rslt (* rslt c))
      (setq c (1+ c)))))

(defun comb1 (n r)
  (/ (fact n)
     (* (fact r) (fact (- n r)))))

(defun fib (n)
  (if (or (= n 0) (= n 1)) 1
      (+ (fib (1- n)) (fib (- n 2)))))

(defun longerp (xs ys)
  (if (null xs) nil
      (if (null ys) t
	  (longerp (cdr xs) (cdr ys)))))

(defun take (xs n)
  (if (null xs) nil
      (if (= 0 n) nil
	  (cons (car xs) (take (cdr xs) (1- n))))))

(defun my-reverse (lst)
  (if (null lst)
      nil
      (append (my-reverse (cdr lst)) (list (car lst)))))
  
      
  
