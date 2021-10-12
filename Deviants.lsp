(defun ptext(x y a p te c ts); point (x,y) a - angle, p - position, te -text, c - color
  (setq a (/ (* a pi) 180))
  (setq le (entmakex
	     '(
	       (0 . "TEXT")
	       (8 . "deviants")
	       (10 0.0 0.0 0.0)
	       (40 . 0.25)
	       (1 . "abc")
	       (50 . 0.0)
	       (7 . "ArialNew")
	       (72 . 9)
	       (62 . 1)
	       (11 0.0 0.0 0.0)
	      )
	     )
	)
  (setq ob (entget le))
  (setq ob (subst (list 10 x y 0.0) (assoc 10 ob) ob))
  (setq ob (subst (list 11 x y 0.0) (assoc 11 ob) ob))
  (setq ob (subst (cons 50 a) (assoc 50 ob) ob))
  (setq ob (subst (cons 1 te) (assoc 1 ob) ob))
  (setq ob (subst (cons 72 p) (assoc 72 ob) ob))
  (setq ob (subst (cons 62 c) (assoc 62 ob) ob))
  (setq ob (subst (cons 40 ts) (assoc 40 ob) ob))
  (entmod ob)
  (entupd le)
)

(defun rnd (/)
  (if (not seed) (setq seed (getvar "DATE")); get date
    )
  (setq mod 65536; module
        mult 25173; multiplicator
        inc 13849; increment
        seed (rem (+ (* mult seed) inc) mod); easy
        random (/ seed mod)
	)
  );defun rnd

(defun rndRange(/); generate numbers in range -10..10
  (setq maxR 20; range
	movR 10; move
	)
  (while (= (setq numSeq (- (fix(* maxR (rnd))) movR)) 0)); generate digits beetween 0..max_r and move result on mov_r under 0
  (if (> numSeq 0)
    (progn
      (setq str (itoa numSeq))
      (setq str (strcat "+" str))
      )
    (setq str (itoa numSeq))
    )
  )

(defun getOb(point sidex sidey / obj); function get object by point and selected side (left, right, top, bottom)
  (setq x1 (nth 0 point) y1 (nth 1 point) x2 (+ (nth 0 point) sidex) y2 (+ (nth 1 point) sidey))
  ; finding object with filter "*LINE" in scene
  (setq s (ssget "_F" (list (list x1 y1)
			    (list x2 y2))
			    (list
			    (cons 0 "*LINE")
			    ;(cons 62 7)
			    ;(cons 6 "Continuous")
			    )
		 )
	)
  
  (if s
    (progn
      (setq obj (entget (ssname s 0))) ; if object exists - return it
  	;(if (and (= (cdr(assoc 62 obj)) 7) (= (cdr(assoc 6 obj)) "Continuous")) (setq obj obj) nil)
      )
    nil
    )
  )

(defun changeObj(objin / objout)
  (if obj ; if exists then continue
    (progn
      (setq obj (subst (cons 8 "blocks") (assoc 8 obj) obj)); replace object into new layer
      (if (assoc 62 obj) ; find parameter named 62 which contain color attribute
	(setq obj (subst (cons 62 5) (assoc 62 obj) obj)); change color
	(setq obj (append obj (list (cons 62 5)))); add color
	)
      )
    )
  )

(defun printext(line1point1 line1point2 line2point1 line2point2 textsize)
  (if (not (setq cen (inters line1point1 line2point2 line1point2 line2point1)))
    (setq cen (inters line1point1 line2point1 line1point2 line2point2))
    )

  (setq dist (min
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
      (ptext x y 0 0 (rndRange) 6 textsize)
      (setq x (min (nth 0 line2point1) (nth 0 line2point2)))
      (setq x (- x (/ dist 6)))
      (ptext x y 0 2 (rndRange) 6 textsize)
      ;--if add center text
      ;(setq x (- (nth 0 cen) (/ textsize 2)) y (- (nth 1 cen) (/ textsize 2)))
      ;(ptext x y 0 1 "№" 7 textsize)
      )
    (progn
      (setq x (min (nth 0 line1point1) (nth 0 line1point2)))
      (setq x (+ (+ x (/ dist 2)) (/ textsize 2)) )
      (setq y (min (nth 1 line1point1) (nth 1 line1point2)))
      (setq y (+ y (/ dist 6)))
      (ptext x y 90 0 (rndRange) 6 textsize)
      (setq y (max (nth 1 line1point1) (nth 1 line1point2)))
      (setq y (- y (/ dist 6)))
      (ptext x y 90 2 (rndRange) 6 textsize)
      ;--if add center text
      ;(setq x (+ (nth 0 cen) (/ textsize 2)) y (- (nth 1 cen) (/ textsize 2)))
      ;(ptext x y 90 1 "№" 7 textsize)
      )
    )
  )

(defun pp4(/)
  (setq txtStyle (getvar "textstyle"))
  (command "._STYLE" "ArialNew" "Arial" 0 1 0 "_N" "_N" "_N")
  (if (not (setq layer (tblsearch "LAYER" "blocks")))
    (command "._layer" "_n" "blocks" "_c" 5 "blocks" "")); add new layer if not exists
  (if (not (setq layer (tblsearch "LAYER" "deviants")))
    (command "._layer" "_n" "deviants" "_c" 6 "deviants" "")); add new layer if not exists
  ;(setq textsize (getreal "Размер текста")) ; TextSize
  ;(if (not textsize) (setq textsize 1))
  (setq textsize 0.25)
  (setq point (getpoint)) ; get point
  
  (while point ; while true do something
    (progn
      (if (setq obj (getOb point -100000 0)); find object in left side
	(progn
	  (setq line1point1 (cdr (assoc 10 obj)))
	  (setq line1point2 (cdr (assoc 11 obj)))
	  (if (= (cdr (assoc 0 obj)) "LINE") ; if object is LINE then find other sides
	    (progn
	      (if (setq obj (getOb point 100000 0)); right side
		(progn
		  (setq line2point1 (cdr (assoc 10 obj)))
		  (setq line2point2 (cdr (assoc 11 obj)))
		  (printext line1point1 line1point2 line2point1 line2point2 textsize)
		  )
		)
	)
	(progn
	  ;(entmod (changeObj obj)) ; change object
	  ;(print obj)
	  (setq i 0); find something???????????????????????????????
	  ;(setq d (list (cons 0 0)))
	  (setq point1 (cdr (assoc 10 obj)) dist 0);
	  
	  (print (cons 0 point1))
	  (setq i (+ (vl-position (assoc 10 obj) obj) 1) )
	  ;(print i)
	  (setq j (+ (vl-position (assoc 10 obj) obj) 1) )
	  (setq c 1)
	  (setq points (list (cons 0 point1)))
	  (while (< j (length obj))
	    (progn
	      (setq ob (nth j obj))
	      (if (= (car ob) 10)
		(progn
		  (setq points (append points (list (cons c (cdr ob)))))
		  (setq c (+ c 1))
		  )
		)
	      (setq j (+ j 1))
	      )
	   )
	  (print points)
	  (print "---------------------")
	  (while (< i (length obj))
	    (progn
	      (setq ob (nth i obj))
	      (if (= (car ob) 10)
		(progn
		  (setq point2 (cdr ob));
		  (print (distance point1 point2));
		  (if (<= dist (distance point1 point2))
		    (progn
		      (setq po1 point1 po2 point2 dist (distance point1 point2))
		      )
		    (progn
		      (if (= (nth 0 po2) (nth 0 point2))
			(progn
			  (setq x1 (/ (- (nth 0 point2) (nth 0 po2)) 2))
			  (setq y1 (/ (- (nth 1 point2) (nth 1 po2)) 2))
			  ;;;;
			  ;(setq x (+ x1 (nth 0 po2)))
			  ;(setq y (+ y1 (nth 1 po2)))
			  ;(ptext x y 0 2 "+10" 1)
			  ;(setq x (+ x1 (nth 0 po1)))
			  ;(setq y (+ y1 (nth 1 po1)))
			  ;(ptext x y 0 9 "-10" 1)
			  ;;;;;;;;
			  (setq x (- (+ x1 (nth 0 po2)) (/ dist 5)))
			  (setq y (- (+ y1 (nth 1 po2)) (/ textsize 2)))
			  (command "._TEXT" (list x y) textsize 0 (rndRange))
			  (command "_.chprop" "_last" "" "_color" 6 "")
			  (command "_.chprop" "_last" "" "_layer" "deviants" "")
			  (setq x (+ (+ x1 (nth 0 po1)) (- (/ dist 5) (/ dist 10))))
			  (setq y (- (+ y1 (nth 1 po1)) (/ textsize 2)))
			  (command "._TEXT" (list x y) textsize 0 (rndRange))
			  (command "_.chprop" "_last" "" "_color" 6 "")
			  (command "_.chprop" "_last" "" "_layer" "deviants" "")
			)
		       )
		      (if (= (nth 1 po2) (nth 1 point2))
			(progn
			  (setq x1 (/ (- (nth 0 point2) (nth 0 po2)) 2))
			  (setq y1 (/ (- (nth 1 point2) (nth 1 po2)) 2))
			  (setq x (+ (+ x1 (nth 0 po2)) (/ textsize 2)))
			  (setq y (+ (+ y1 (nth 1 po2)) (- (/ dist 5) (/ dist 10))))
			  (command "._TEXT" (list x y) textsize 90 "3")
			  (command "_.chprop" "_last" "" "_color" 6 "")
			  (command "_.chprop" "_last" "" "_layer" "deviants" "")
			  (setq x (+ (+ x1 (nth 0 po1)) (/ textsize 2)))
			  (setq y (- (+ y1 (nth 1 po1)) (/ dist 5)))
			  (command "._TEXT" (list x y) textsize 90 "4")
			  (command "_.chprop" "_last" "" "_color" 6 "")
			  (command "_.chprop" "_last" "" "_layer" "deviants" "")
			)
		       )
		      )
		    )
		  (setq point1 point2);
		)
	       )
	      (setq i (+ i 1))
	    )
	  )
	  (print (distance po1 po2))
	  
	  ;(print d)
	)
      )
      )
	)
  (setq point (getpoint)) ; get new point
  )
 )
    (setvar "textstyle" txtStyle)
  (princ)
)

  (pp4)
