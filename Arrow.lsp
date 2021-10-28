(defun ptext(x y a p te c ts l); point (x,y) a - angle, p - position, te -text, c - color, ts - textsize, l - layer
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
	       (11 0.0 0.0 0.0)))
	)
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
  )

(defun rnd (/) ; generate random digits
  (if (not seed) (setq seed (getvar "DATE")) ); get date
  (setq mod 65536; module
        mult 25173; multiplicator
        inc 13849; increment
        seed (rem (+ (* mult seed) inc) mod); easy
        random (/ seed mod))
  )

(defun rndRange(/); generate numbers in range 0..18
  (setq maxR 18; range
	movR 0); move
  (while (zerop (setq numSeq (- (fix(* maxR (rnd))) movR)))); generate digits beetween 0..max_r and move result on mov_r under 0
  (setq str (itoa numSeq)) ; result to string format
  )

(defun c:arrow (/)
  (setq txtStyle (getvar "textstyle")) ; get current textstyle
  (command "._STYLE" "ArialNew" "Arial" 0 1 0 "_N" "_N" "_N") ; create new textstyle
  (if (not (setq layer (tblsearch "LAYER" "x_deviants")))
    (command "._layer" "_n" "x_deviants" "_c" 6 "x_deviants" "")); add new layer if not exists
  
  (setq textsize 0.25) ; textsize
  (initget 1) (setq point (getpoint "\nТочка вставки:")) ; get point to insert arrow
  (while point ; while point is not false
    (progn
      (initget 1 "Левая Правая") ; get orientation of arrow
      (if (not (setq orient (getkword "\nНаправление: [Левая/Правая] <Левая>")))
	(setq orient "Левая") ; set default orientation
	)
      (setq x (nth 0 point)  ; get coordinates
	    y (nth 1 point)) ; for x, y
      (setq line (entmakex ; create arrows
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
      
      (if (= orient "Левая") ; check orient
	(progn
	  (command "._rotate" line "" point "_R" 180 0) ; if left, then rotate arrows by 180 degrees
	  (ptext (- x (* 6.5 0.2)) (+ y 0.2) 0 0 (rndRange) 6 textsize "x_deviants")       ; create digits
	  (ptext (- x (* 1 0.15)) (+ y (* 5 0.2)) 0 2 (rndRange) 6 textsize "x_deviants")  ; for arrows
	  )
	(progn
	  (ptext (+ x (* 6.5 0.2)) (- y (* 2 0.2)) 0 2 (rndRange) 6 textsize "x_deviants") ; create digits
	  (ptext (+ x (* 1 0.15)) (- y (* 6.5 0.2)) 0 0 (rndRange) 6 textsize "x_deviants"); for arrows
	  )
	)
      
      (initget 1) (setq point (getpoint "\nТочка вставки:")) ; get new point for WHILE
      )
    )
  (setvar "textstyle" txtStyle) ; set textstyle
  (princ)
  )
;(arrow)