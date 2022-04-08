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
