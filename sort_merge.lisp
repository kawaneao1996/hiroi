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
;;実行時間はN^2に比例
(defun insert-sort (xs pred)
  (if (null xs)
      nil
      (insert-element (car xs) (insert-sort (cdr xs) pred) pred)))

;;クイックソート
;;実行時間は平均してN*log2(N)に比例。ただしリストがソート済みに近い状態だと最悪N^2くらいになる
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
;;マージとは二つのソート済みのリストを統合するやり方
;;平均してN*log2(N)の実行時間。クイックソートと違いリストの内容に依らない
(defun merge-list (xs ys pred)
  (cond
    ((null xs) ys)
    ((null ys) xs)
    ((funcall pred (car xs) (car ys))
     (cons (car xs) (merge-list (cdr xs) ys pred)))
    (t (cons (car ys) (merge-list xs (cdr ys) pred)))))

(defun merge-sort (xs n pred)
  (cond
    ((null xs) nil)
    ((= n 1) (list (car xs)))
    (t (let ((m (floor n 2)))
	 (merge-list (merge-sort xs m pred)
		     (merge-sort (nthcdr m xs) (- n m) pred)
		     pred)))))
     
		     
