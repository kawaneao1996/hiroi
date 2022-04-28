;;with-open-fileの具体例
(with-open-file (out "test.txt" :direction :output)
  (dotimes (x 10) (print x out)))

;;with-open-fileの具体例２
(with-open-file (in "test.txt" :direction :input)
  (loop (let ((num (read in nil)))
	  (if (not num) (return))
	  (print num))))

;;read-line : ストリームから文字列を読み込む。開業は含まない。
(with-open-file (in "test1.txt" :direction :input)
  (loop (let ((buff (read-line in nil)))
	  (if (not buff) (return))
	  (print buff))))

;;read-char : read-lineの一文字版
(with-open-file (in "test1.txt" :direction :input)
  (loop (let ((buff (read-char in nil)))
	  (if (not buff) (return))
	  (print buff))))
;;write-line : 文字列をストリームに書き込む（改行あり）
(dotimes (x 10) (write-line "hello!"))
;;write-string : 改行なし
;;:startと:endが使える
(dotimes (x 10) (write-char (code-char 101)))

;;read-sequence : 列にデータを読み込む,副作用あり

;;write-sequence : 列にデータを書き込む

;;stream-element-type : ストリームのデータ型を調べる

(defvar buff (make-array '(5) :initial-element 0))

(read-sequence buff *standard-input*)

(write-sequence buff *standard-output*)

(read-sequence buff *standard-input* :start 1 :end 4)

(write-sequence buff *standard-output* )

;;(ungsigned-byte n) : 符号無しのバイト

;;問題1
;;cat-file-test : (cat-file-test "lambda.lisp" "sequence.lisp" "test.txt")で動いた
(defun cat-file-test (fname &rest fname-lst)
  (let ((f fname))
    (with-open-file (in f :direction :input)
      (loop (let ((txt (read in nil)))
	      (if (not txt) (return))
	      (print txt))))
    (if fname-lst
	(dolist (g fname-lst )
	  (with-open-file (in g :direction :input)
	    (loop (let ((txt (read in nil)))
		    (if (not txt) (return))
		    (print txt))))
	  ))))
 (cat-file-test "lambda.lisp" "sequence.lisp" "test.txt")
;;セミコロンなしでファイル名を渡したい：下は引数fnを文字列にしようとして失敗
;;quoteしてる時点で引数が評価されないため
;; (defun cf (fn)
;;   (with-open-file (in (symbol-name (quote fn)) :direction :input)
;;     (loop (let ((num (read in nil))) (if (not num) (return)) (print num)))))

;;下はprincで引数を文字列にしようとして失敗
;;そもそもprinc　の引数は文字列だった
;; (defun cf1 (fname)
;;   (let* ((fn (princ fname)))
;;     (with-open-file (in fn :direction :input)
;;       (loop (let ((num (read in nil))) (if (not num) (return)) (print num))))
;;     ))

;;問題２
(defun head-file (fname)
  (with-open-file (in fname :direction :input)
    (dotimes (i 10)
      (let ((txt (read in nil)))
	(if (not txt) (return)
	    (print txt))))))
	       

(head-file "sequence.lisp")

;;問題３
;;なんか動かない

(defun tail-file (fname)
  (with-open-file (in fname :direction :input)
    (let ((rslt nil))
      (loop  (let ((txt (read in nil)))
	       (if (not txt) (return)
		   (concatenate 'list rslt txt))))
      (if (> (length rslt) 10)
	  (dolist  (it (reverse (subseq rslt 0 10)))
	    (print it))

	  ;;else
	  (dolist  (ij (reverse (subseq rslt 0 )))
	    (print ij))))))

;;(tail-file "lambda.lisp")

;;解けなかったのので解答を見た
(defun tail-file (file)
  (let (buff queue)
    (with-open-file
	(in file :direction :input)
      (loop
       (if (not (setq buff (read-line in nil)))
	   (return))
       (setq queue
	     (append (if (< (length queue) 10) queue (cdr queue))
		     (list buff)))))
    (dolist (xs queue) (write-line xs))))
		
	 
