;;cows :数字は合っているが位置が違う
;;bulls:数字も位置も合っている

(defun make-answer ()
  (do ((ans nil)
       (n (random 10) (random 10)))
      ((= (length ans) 4) ans)
    (unless (member n ans)
      (push n ans))))

(defun check-input-code (code)
  (and (consp code)
       (= (length code) (length (remove-duplicates code)) 4)
       (every (lambda (x) (and (integerp x) (<= 0 x 9))) code)))

(defun input-code ()
  (loop
    (princ ">>> ")
    (finish-output)
    (let ((code (read)))
      (if (check-input-code code)
	  (return code))
      (format t "異なる４つの数字(0-9)を入力してください~%"))))
	       

;;bullsをカウントする
(defun count-bulls (answer code)
  (count t (mapcar (function =) answer code)))

;;同じ文字をカウントする
(defun count-same-number (answer code)
  (let ((c 0))
    (dolist (x code c)
      (if (member x answer) (incf c)))))

;;ゲーム本体
(defun mastermind ()
  (let ((ans (make-answer)))
    (dotimes (x 10 (format t "残念！正解は~aでした" ans))
      (let* ((code (input-code))
	     (bulls (count-bulls ans code))
	     (cows (- (count-same-number ans code) bulls)))
	(format t "~2d: ~a, bulls = ~d, cows = ~d~%"
		(1+ x) code bulls cows)
	(when (= bulls 4)
	  (format t "おめでとう！！~%")
	  (return))))))
	     
	       
