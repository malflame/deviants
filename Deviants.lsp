(defun C:Deviants(/) ; main function

  (setq txtStyle (getvar "textstyle"))
  (command "._STYLE" "ArialNew" "Arial" 0 1 0 "_N" "_N" "_N")
  (if (not (setq layer (tblsearch "LAYER" "x_deviants")))
    (command "._layer" "_n" "x_deviants" "_c" 6 "x_deviants" "")); add new layer if not exists
  (if (not (setq layer (tblsearch "LAYER" "x_blocknumber")))
    (command "._layer" "_n" "x_blocknumber" "_c" 6 "x_blocknumber" "")); add new layer if not exists
  ;(setq textsize (getreal "Введите размер текста:")) ; TextSize
  ;(if (not textsize) (setq textsize 0.25))
  (setq textsize 0.25)

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
    ; changing parameters for text object
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
    (setq maxR 38; range
	  movR 19; move
	  )
    (while (zerop (setq numSeq (- (fix(* maxR (rnd))) movR)))); generate digits beetween 0..max_r and move result on mov_r under 0
    (if (> numSeq 0)
      (progn
	(setq str (itoa numSeq))
	(setq str (strcat "+" str))
	)
      (setq str (itoa numSeq))
      )
    ); end rndRange


  (defun getPair(d1 / str); find pair for already generated digit
    (setq d2 (atoi (rndRange)))
    (while (and (< (* d1 d2) 0) (>= (+ (abs d1) (abs d2)) 20)) (setq d2 (atoi (rndRange))))
    (if (> d2 0)
      (progn
	(setq str (itoa d2))
	(setq str (strcat "+" str))
	)
      (setq str (itoa d2))
      )
    ); end getPair

  (defun getOb(point sidex sidey / obj); function get object by point and selected side (left, right, top, bottom)
    (setq x1 (nth 0 point) y1 (nth 1 point) x2 (+ (nth 0 point) sidex) y2 (+ (nth 1 point) sidey))
    ; finding object with filter "*LINE" in scene
    (setq s (ssget "_F" (list (list x1 y1)
			    (list x2 y2))
			    (list
			    (cons 0 "*LINE") )
		 )
	  )

    (if s (setq obj (entget (ssname s 0))) nil)
    ); end getObj

  (defun printext(line1point1 line1point2 line2point1 line2point2 textsize)
    (if (not (setq cen (inters line1point1 line2point2 line1point2 line2point1)))
      (setq cen (inters line1point1 line2point1 line1point2 line2point2))
      )
    (setq dist (min ; finding minimum distance
		 (distance line1point1 line2point1)
		 (distance line1point1 line2point2)
		 (distance line1point1 line1point2)
		 (distance line2point1 line2point2)
		 )
	  )
    (if (= dist (min (distance line1point1 line1point2) (distance line2point1 line2point2)))
      (progn
	(setq x (min (nth 0 line1point1) (nth 0 line1point2))); find bottom X
	(setq x (+ x (/ dist 6)))
	(setq y (min (nth 1 line1point1) (nth 1 line1point2)))
	(setq y (- (+ y (/ dist 2)) (/ textsize 2)) )
	(setq dig (rndRange))
	(ptext x y 0 0 dig 6 textsize "x_deviants")
	(setq x (min (nth 0 line2point1) (nth 0 line2point2)))
	(setq x (- x (/ dist 6)))
	(ptext x y 0 2 (getPair (atoi dig)) 6 textsize "x_deviants")
	;--if add center text
	(setq x (- (nth 0 cen) (/ textsize 2)) y (- (nth 1 cen) (/ textsize 2)))
	(ptext x y 0 1 "####" 7 textsize "x_blocknumber")
	)
      (progn
	(setq x (min (nth 0 line1point1) (nth 0 line1point2)))
	(setq x (+ (+ x (/ dist 2)) (/ textsize 2)) )
	(setq y (min (nth 1 line1point1) (nth 1 line1point2)))
	(setq y (+ y (/ dist 6)))
	(setq dig (rndRange))
	(ptext x y 90 0 dig 6 textsize "x_deviants")
	(setq y (max (nth 1 line1point1) (nth 1 line1point2)))
	(setq y (- y (/ dist 6)))
	(ptext x y 90 2 (getPair (atoi dig)) 6 textsize "x_deviants")
	;--if add center text
	(setq x (+ (nth 0 cen) (/ textsize 2)) y (- (nth 1 cen) (/ textsize 2)))
	(ptext x y 90 1 "####" 7 textsize "x_blocknumber")
	)
      )
    ); end printext


  (setq point (getpoint)) ; get point
  (while point ; while true do something
    (progn
      (if (setq obj (getOb point -1000000 0)); find object in left side
	(progn
	  (setq line1point1 (cdr (assoc 10 obj)))
	  (setq line1point2 (cdr (assoc 11 obj)))
	  (if (= (cdr (assoc 0 obj)) "LINE") ; if object is LINE then find right side and print text
	    (progn
	      (if (setq obj (getOb point 1000000 0)); right side
		(progn
		  (setq line2point1 (cdr (assoc 10 obj)))
		  (setq line2point2 (cdr (assoc 11 obj)))
		  (setq obj (getOb point 0 -1000000)) ; bottom
  		  (setq line3point1 (cdr (assoc 10 obj)))
		  (setq line3point2 (cdr (assoc 11 obj)))
		  (setq obj (getOb point 0 1000000)) ; top
  		  (setq line4point1 (cdr (assoc 10 obj)))
		  (setq line4point2 (cdr (assoc 11 obj)))
		  (setq l1p1 (inters
			       (list (nth 0 line1point1) -1000000 0.0) (list (nth 0 line1point1) 1000000 0.0)
			       (list -1000000 (nth 1 line3point1) 0.0) (list 1000000 (nth 1 line3point1) 0.0)
			       )
			)
		  (setq l1p2 (inters
			       (list (nth 0 line1point1) -1000000 0.0) (list (nth 0 line1point1) 1000000 0.0)
			       (list -1000000 (nth 1 line4point1) 0.0) (list 1000000 (nth 1 line4point1) 0.0)
			       )
			)
		  (setq l2p1 (inters
			       (list (nth 0 line2point1) -1000000 0.0) (list (nth 0 line2point1) 1000000 0.0)
			       (list -1000000 (nth 1 line3point1) 0.0) (list 1000000 (nth 1 line3point1) 0.0)
			       )
			)
		  (setq l2p2 (inters
			       (list (nth 0 line2point1) -1000000 0.0) (list (nth 0 line2point1) 1000000 0.0)
			       (list -1000000 (nth 1 line4point1) 0.0) (list 1000000 (nth 1 line4point1) 0.0)
			       )
			)
		  (printext l1p1 l1p2 l2p1 l2p2 textsize)
		  )
		)
	      )
	    (progn ; if object not LINE
	      (setq point1 (cdr (assoc 10 obj)) dist 0);
	      (setq i (+ (vl-position (assoc 10 obj) obj) 1) ) ; counter
	      (setq c 1) ; counter for list "points"
	      (setq points (list (cons 0 (cdr (assoc 10 obj))))) ; add list with first point
	      (while (< i (length obj))
		(progn
		  (setq ob (nth i obj))
		  (if (= (car ob) 10)
		    (progn
		      (setq points (append points (list (cons c (cdr ob))))) ; append new point into "points"
		      (setq c (+ c 1))
		      )
		    )
		  (setq i (+ i 1))
		  )
		)
	      (setq points (vl-sort points (function
					     (lambda (v1 v2)
					       (<= (nth 1 v1) (nth 1 v2))))))
	      (if (= (length points) 4)
		(printext (cdr (nth 0 points))
			  (cdr (nth 1 points))
			  (cdr (nth 2 points))
			  (cdr (nth 3 points)) textsize)
		)
	      )
	    )
	  )
	)
      (setq point (getpoint)) ; get new point
      )
    )
  (setvar "textstyle" txtStyle)
  ); end c:deviants
