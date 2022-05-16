;;ランダムな数列を返す
(defun random-seq (n)
  (do ((ans nil  (remove-duplicates ans)))
      ((>= (length ans) n) ans)
    (setq ans (cons (random (1+ n)) ans))
    ))
;;データをひとつのリストに挿入
(defun insert-element (x xs pred)
  (cond
    ((null xs) (list x))
    ((funcall pred x (car xs)) (cons x xs))
    (t (cons (car xs)
	     (insert-element x (cdr xs) pred)))))

;;;挿入ソート
(defun insert-sort (xs pred)
  (if (null xs)
      nil
      (insert-element (car xs) (insert-sort (cdr xs) pred) pred)))

;;クイックソート
;;リストの分割
(defun partition (p xs pred)
  (let  (ys zs)
    (dolist (x xs (list ys zs))
      (if (funcall pred x p) (push x ys) (push x zs)))))

(defun quick-sort (xs pred)
  (if (null xs)
      nil
      (let ((ys (partition (car xs) (cdr xs) pred)))
	(append (quick-sort (first ys) pred)
		(list (car xs))
		(quick-sort (second ys) pred)))))

			   
