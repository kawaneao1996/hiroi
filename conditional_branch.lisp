;;case の例
(defun area (x r)
  (case
      x
    (square (* r r))
    (cube (* r r r))
    (circle (* pi r r))
    (ball (* 4 pi r r ))
    (t 0)))

(area 'cube 100)

(defun fizzbuzz (x)
  (cond
    ((zerop (mod x 15)) 'fizzbuzz)
    ((zerop (mod x 3)) 'fizz)
    ((zerop (mod x 5)) 'buzz)
    (t x)))
     
