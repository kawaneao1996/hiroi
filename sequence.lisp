;;sequence = list + string + vector
;;elt sequence index
(elt '(11 12 13 14 15) 0)
;;subseq seq start [end]
(subseq #(11 12 13 14 15) 1 4)
;;copy-seq seq
(copy-seq '(1 2 3 4))
;;length seq
(length "I am a person")
;;reverse seq
(reverse #(1 2 3 4 5 6 7))
;;make-sequence type size
(make-sequence 'vector 19 :initial-element pi)
;;char string index
(char "abcdeあいうえお" 7)
;;列の探索
;;find item seq
(find 'pi '(11 21 31 41 pi 0 nil) :test #'equal)
(find pi `(1 2 3 4 5 ,pi))
;;find-if(-not) predicate sequence
(find-if #'identity '(1 2 3 4 5 6))
;;position item seq
(position 12 #(10 19 18 17 16 15 14 13 12 11))
;;position-if(-not) predicate sequence
(position-if (function zerop) '(1 2 3 4 5 6 7 8 9 0))
;;count item sequence
(count 'a #(a b ab b a a s d f c d))
;;count-if(-not) predicate sequence
(count-if (function null) '(1 2 3 nil 4 5))
(count-if #'null (append '(1 2 3 nil) nil)) ; = 1
;;kerwords
;; :start :end
(find-if #'oddp '(10 11 12 13 14 15 16 17 18 19) :start 2)
;; :test :test-not
(find "def" '("abc" "def" "ghi") :test #'equal)
;; :key
(find 'b `(,(cons 'a 1) ,(cons 'b 2) (c . 3)) :key #'car)
;; :count
(remove-if #'oddp '(1 2 3 4 5 6 7 8 9 0 11 12 13) :count 3)
;; :from-end
(remove-if #'oddp '(1 2 3 4 5 6 7 8 9 0 11 12 13) :count 3 :from-end t)
;;remove item sequence
(defparameter *vec* #(1 2 3 4 5 6 a b c d e f 7 8 9))
(remove 'b *vec*)
*vec*
;;remove-if(-not) predicate sequence
(defun mklst2 (s e)
  (do ((i s (1+ i)) (rslt nil))
      ((> i e) rslt)
    (setq rslt (append rslt (list i)))))
(defun mklst (s e)
  (do ((i s (1+ i)) (rslt nil))
      ((> i e) (reverse rslt))
    (push i rslt)))
(remove-if #'oddp (mklst 11 20))

;;substitute new old sequence :oldと等しい要素をnewに置き換える
(substitute 13 12 (mklst 1 20))

;;substitute-if(-not) new predicate sequence : predicateを満たす要素を置き換え
(substitute-if 0 #'oddp (mklst 1 20))

;;fill sequence item : 列の要素をitemで置き換える
(fill (mklst 1 20) pi :start 4 )
(fill (mklst 1 20) 99 :start 4 :end 7)
;;remove-duplicates sequence : 列の重複した要素を取り除く
(remove-duplicates (fill (mklst 1 20) 99 :start 4 :end 7))

(defun mkid (n element)
  (let ((rslt nil))
    (dotimes (i n rslt)
      (push element rslt))))

;; :count は処理する要素の個数を制限
(remove 9  (mkid 20 9))
(remove 9  (mkid 20 9) :count 3)
(length (remove 9  (mkid 20 9) :count 3)) ; = 17
(remove 9 (append (mklst 1 20) (mkid 10 9) (mklst 11 15)) :count 6)

;;dot対のリストを作る
(defun mkdots2 (lst1 lst2)
  (if (and (null lst1) (null lst2))
	nil
	(cons (cons (car lst1) (car lst2))
	      (mkdots2 (cdr lst1) (cdr lst2)))))
	     
(defun mkdots (lst1 lst2)
  (let ((rslt nil) (count (min (length lst1) (length lst2))))
    (dotimes (i count (reverse rslt))
      (push (cons (nth i lst1) (nth i lst2)) rslt))))

(remove 'b (mkdots '(a b a b a b a b a b) (mklst 1 10) ) :count 2 :from-end t :key #'car)

;;remove-dupricates は等しい要素の前から消していく
(remove-duplicates (mkdots '(a b a b a b a b a b) (mklst 1 10) ) :key #'car ) ;((A . 9) (B . 10))

;;map result-type func sequence ... : 列の要素にfuncを適用し結果を列に格納して返す
(map 'list (function mklst) '(0 1 2 3 4 5 6 7) '(3 4 5 6 7 8 9 10))
(map 'list (function cons) (mklst 1 7) (mklst 12 18))
;;map-into result-seq func sequence ... : 列の要素にfuncを適用しresult-seqに代入する result-seq の次元は変化しない！！
(defparameter result-seq nil)
(defun flutten (lst &rest lst2)
  (cond ((and (null lst) (null lst2)) nil)
	((and (null lst) (not (null lst2))) (flutten lst2))
	((null lst2)
	 (if (null lst) nil
	     (if (atom lst) (list lst)
		 (append (flutten (car lst)) (flutten (cdr lst))))))
	(t (append (flutten (car lst)) (flutten (cdr lst) lst2)))))
(map-into result-seq
	  #'flutten
	  (mkdots (mklst 1 10) (mklst 10 20))
	  (mkdots (mklst -9 0) (mklst 1.1 10.1)))

	  
(flutten
     (mkdots (mklst 1 10) (mklst 10 20))
     (mkdots (mklst -9 0) (mklst 1.1 10.1)))

(map-into result-seq #'flutten 
	  (list (mkdots (mklst 1 10) (mklst 10 20))
		(mkdots (mklst -9 0) (mklst 1.1 10.1)))
	  (list (mkdots (mklst 1 10) (mklst 10 20))
		(mkdots (mklst -9 0) (mklst 1.1 10.1)))
	  )

(map-into result-seq #'+ 
	  (flutten (mkdots (mklst 1 10) (mklst 10 20))
		(mkdots (mklst -9 0) (mklst 1.1 10.1)))
	  (flutten (mkdots (mklst 1 10) (mklst 10 20))
		(mkdots (mklst -9 0) (mklst 1.1 10.1)))
	  ) ;=> #(2 20 4)
(setq result-seq (make-array 50 :initial-element pi))
(map-into result-seq #'+ 
	  (flutten (mkdots (mklst 1 10) (mklst 10 20))
		(mkdots (mklst -9 0) (mklst 1.1 10.1)))
	  (flutten (mkdots (mklst 1 10) (mklst 10 20))
		(mkdots (mklst -9 0) (mklst 1.1 10.1)))
	  )
;;=>#(2 20 4 22 6 24 8 26 10 28 12 30 14 32 16 34 18 36 20 38 -18 2.2 -16 4.2 -14  6.2 -12 8.2 -10 10.2 -8 12.2 -6 14.2 -4 16.2 -2 18.2 0 20.2  3.141592653589793d0 3.141592653589793d0 3.141592653589793d0  3.141592653589793d0 3.141592653589793d0 3.141592653589793d0  3.141592653589793d0 3.141592653589793d0 3.141592653589793d0  3.141592653589793d0)

