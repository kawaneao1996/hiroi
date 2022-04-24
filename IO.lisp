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

(write-sequence buff *standard-output*)
