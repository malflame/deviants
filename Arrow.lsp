(defun c:arrow(/)
  
  (setq txtStyle (getvar "textstyle"))
  (command "._STYLE" "ArialNew" "Arial" 0 1 0 "_N" "_N" "_N")
  (if (not (setq layer (tblsearch "LAYER" "x_deviants")))
    (command "._layer" "_n" "x_deviants" "_c" 6 "x_deviants" "")); add new layer if not exists
  (setq textsize 0.25)
  
  (defun ptext(x y a p te c ts l / le); point (x,y) a - angle, p - position, te -text, c - color, ts - textsize, l - layer
    (setq a (/ (* a pi) 180))
    (setq le (entmakex
	     '((0 . "TEXT")
	       (8 . "x_deviants")
	       (10 0.0 0.0 0.0)
	       (40 . 0.25)
	       (1 . "abc")
	       (50 . 0.0)
	       (7 . "ArialNew")
	       (72 . 9)
	       (62 . 1)
	       (11 0.0 0.0 0.0))))
    (setq ob (entget le))
    (setq ob (subst (list 10 x y 0.0) (assoc 10 ob) ob))
    (setq ob (subst (list 11 x y 0.0) (assoc 11 ob) ob))
    (setq ob (subst (cons 50 a) (assoc 50 ob) ob))
    (setq ob (subst (cons 1 te) (assoc 1 ob) ob))
    (setq ob (subst (cons 72 p) (assoc 72 ob) ob))
    (setq ob (subst (cons 62 c) (assoc 62 ob) ob))
    (setq ob (subst (cons 40 ts) (assoc 40 ob) ob))
    (setq ob (subst (cons 8 l) (assoc 8 ob) ob))
    (entmod ob) (entupd le)
    ); end ptext

  (defun rnd(/)
    (if (not seed) (setq seed (getvar "DATE")); get date
      )
    (setq mod 65536; module
	  mult 25173; multiplicator
	  inc 13849; increment
	  seed (rem (+ (* mult seed) inc) mod); easy
	  random (/ seed mod)
	  )
    ); end rnd

  (defun rndRange(/); generate numbers in range
    (setq maxR 18; range
	  movR 0; move
	  )
    (while (zerop (setq numSeq (- (fix(* maxR (rnd))) movR)))); generate digits beetween 0..max_r and move result on mov_r under 0
    (setq str (itoa numSeq))
    ); end rndRange

  (initget 1) (setq point (getpoint "\nТочка вставки:"))
  (while point
    (progn
      (initget 1 "Левая Правая")
      (if (not (setq orient (getkword "\nНаправление: [Левая/Правая] <Левая>")))
	(setq orient "Левая")
	)
      (setq x (nth 0 point) y (nth 1 point))
      (setq line (entmakex
	       (list
		 (cons 0  "LWPOLYLINE")
		 (cons 8 "x_windows_doors")
		 (cons 100 "AcDbEntity")
		 (cons 100 "AcDbPolyline")
		 (cons 90 11)
		 (cons 70 0)
		 (cons 62 6)
		 (list 10 (+ x (* 5.8 0.2)) y)
		 (list 10 (+ x (* 4 0.2)) (+ y 0.15))
		 (list 10 (+ x (* 6 0.2)) y)
		 (list 10 (+ x (* 4 0.2)) (- y 0.15))
		 (list 10 (+ x (* 5.8 0.2)) y)
		 (list 10 x y)
		 (list 10 x (- y (* 5.8 0.2)))
		 (list 10 (- x 0.15) (- y (* 4 0.2)))
		 (list 10 x (- y (* 6 0.2)))
		 (list 10 (+ x 0.15) (- y (* 4 0.2)))
		 (list 10 x (- y (* 5.8 0.2)))
		 )
	       )
	    )
      (if (= orient "Левая")
	(progn
	  (command "._rotate" line "" point "_R" 180 0)
	  (setq le1 (ptext (- x (* 6.5 0.2)) (+ y 0.2) 0 0 (rndRange) 6 textsize "x_deviants"))
	  (setq le2 (ptext (- x (* 1 0.15)) (+ y (* 5 0.2)) 0 2 (rndRange) 6 textsize "x_deviants"))
	  (command "._group" "" "*" "" line le1 le2 "")
	  )
	(progn
	  (setq le1 (ptext (+ x (* 6.5 0.2)) (- y (* 2 0.2)) 0 2 (rndRange) 6 textsize "x_deviants"))
	  (setq le2 (ptext (+ x (* 1 0.15)) (- y (* 6.5 0.2)) 0 0 (rndRange) 6 textsize "x_deviants"))
	  (command "._group" "" "*" "" line le1 le2 "")
	  )
	)
      
      (initget 1) (setq point (getpoint "\nТочка вставки:"))
      )
    )
  (setvar "textstyle" txtStyle)
  ); end c:arrow
