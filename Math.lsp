(defclass math-operations ()
  ((subresults :accessor m91sub :initform nil)))

(defun bubble-pulsation (a p0 rho r0)
  (let ((adiabatic (* (/ 1 (* 2 pi r0))
		      (sqrt (/ (* 3 a p0) rho))))
	(isothermal (* (/ 1 (* 2 pi r0))
		       (sqrt
			(+ (/ (* 3 p0) rho)
			   (/ (* 4 a) (* rho r0)))))))
    (values adiabatic isothermal)))

;; chaotic functions

;; this function implements a logistic map
(defun logistic-map (x r)
  (let* ((r1 (cond ((< r 0) 0)
		   ((>= r 4) (- 4 1e-6))
		   (t r))))
    (* x r1 (- 1 x))))
;; Linear Interpolation
(defmethod scale-value (value oldMin oldMax newMin newMax)
  (+ (floor (* (- value oldMin)
	   (- newMax newMin)) (- oldMax oldMin)) newMin))
;; Runs the logistic map n times, starting from an initial x0
;; value and then scales it to (min max) values
;; no error checking
(defun logistic-curve (x0 r n)
  (let* ((xi (cond ((< x0 0) 0)
		   ((> x0 1) 1)
		   (t x0))))
    (loop repeat n
       for x = (logistic-map xi r)
       then (logistic-map x r)
       collect x)))



;; lorenz system
(defmethod lorenz-system (n sigma beta ro init)
    (loop repeat n
       with (x0 y0 z0) float = '(1.0 1.0 1.0)
       for x = (+ x0 (* init (* sigma (- y0 x0))))
       for y = (+ y0 (* init (- (* x0 (- ro z0)) y0)))
       for z = (+ z0 (* init (- (* x0 y0) (* z0 beta))))
       collect (list x y z)
       do
	 (setf x0 x
	       y0 y
	       z0 z)))

(defmethod rossler-attractor (n a b c inc &key (x1 1.0) (y1 1.0) (z1 1.0))
  (loop repeat n
     with (x0 y0 z0) float = (list x1 y1 z1)
     for x = (+ x0 (* inc (- (- y0) z0)))
     for y = (+ y0 (* inc (+ x0 (* a y0))))
     for z = (+ z0 (* inc (+ b (* z0 (- x0 c)))))
     collect (list x y z)
     do
       (setf x0 x
	     y0 y
	     z0 z)))

(defun double-pendulum (n m1 m2 l1 l2 t1 t2 &key (time 0.01) (g 9.81))
  (loop repeat n
     with theta1 = t1
     with theta2 = t2
     for delta = (- theta1 theta2)
     with d1theta1 = 0.0
     with d1theta2 = 0.0
     for d2theta1 = (/ (+ (* m2 l1 (expt d1theta1 2)
			     (sin delta) (cos delta))
			  (* m2 g (sin theta2) (cos delta))
			  (* m2 l2 (expt d1theta2 2) (sin delta))
			  (- (* (+ m1 m2) g (sin theta1))))
		       (- (* l1 (+ m1 m2))
			  (* m2 l2
			     (expt (cos delta) 2))))
     for d2theta2 = (/ (+ (- (* m2 l2 (expt d1theta2 2)
				(sin delta) (cos delta)))
			  (* (+ m1 m2)
			     (- (* g (sin theta1) (cos delta))
				(* l1 (expt d1theta1 2) (sin delta))
				(* g (sin theta2)))))
		       (- (* (+ m1 m2) l2)
			  (* m2 l2
			     (expt (cos delta) 2))))
     for x1 = (* l1 (sin theta1))	; in radians
     for y1 = (- (* l1 (cos theta1)))
     for x2 = (+ (* l1 (sin theta1)) (* l2 (sin theta2)))
     for y2 = (- (- (* l1 (cos theta1))) (* l2 (cos theta2))) 
     collect (list x1 y1 x2 y2)
     do
       (setf d1theta1 (+ d1theta1 (* time d2theta1))
	     d1theta2 (+ d1theta2 (* time d2theta2))
	     theta1 (+ theta1 (* time d1theta1))
	     theta2 (+ theta2 (* time d1theta2)))))

(defun calculate-total-time (measures beatunit tempo)
  (/ (* measures beatunit) tempo))

(defun calculate-measures (total tempo beatunit)
  (/ (* total tempo) beatunit))

(defun calculate-beat-unit (total measures tempo)
  (/ (* total tempo) measures))

(defun calculate-tempo (total measures beatunit)
  (/ (* measures beatunit) total))
;; create a bar from a set of single durations
;; fill the empty spaces with rests
;; todo: what abour ties? include an option to tie
;; todo: triplets and nested rhythms
;; post: return the bars and any leftover 
(defun create-bars (rhythms timesig &key (sequential nil))
  (let* ((limit (duration timesig))
	 (rhythm-durations (mapcar #'duration rhythms))
	 (bar-index (loop for index from 0 below (length rhythms)
		       collect (loop for i from index below (length rhythms)
				  for d = (nth i rhythm-durations)
				  for r = (nth i rhythms)
				  for sum = d then (+ sum d)
				  while (<= sum limit)
				  collect i
				  finally
				    (if (not sequential)
					(setf index (1- i))))))
	 (complete-bars (loop for b in bar-index
	 		   for dur = (reduce #'+
	 				     (loop for i in b
	 					collect (nth i
							     rhythm-durations)))
			   collect
			     (loop for i in b
				collect (nth i rhythms) into bars
				finally
				  (return
				    (if (< dur limit)
					(append bars
						(list 
						 (make-rhythm (rq
							       (make-rhythm
								(rationalize
								 (- limit
								    dur)))))))
					bars)))))
	 (firstbar (append (list (data timesig))
			   (first complete-bars)))
	 (rspbar (append (list firstbar)
			 (loop for i in complete-bars
			    collect i)))
	 (databars (loop for i in complete-bars
		      collect (mapcar #'(lambda (x)
					  (if (is-rest x)
					      (list (data x))
					      (data x))) i))))
    ;; (print rspbar)
    (values rspbar complete-bars databars)))

;; create a method that generates a rhythm hash table when needed
(defun rhythm-hash (keys rhythms)
  (if (= (length keys) (length rhythms))
      (let* ((mymap (make-hash-table :size (length keys))))
	(loop for k in keys
	   for r in rhythms
	   do
	     (setf (gethash k mymap) r))
	mymap)
      nil))

(defun values-to-rhythms (rthm-values)
  (map 'list #'(lambda (x) (if (or (symbolp x)
				   (numberp x))
			       (make-rhythm x)
			       (make-rhythm (pop x) :is-rest t))) rthm-values))
(defun get-attacks (bar)
  (reduce #'+ (mapcar
	       #'(lambda (x) (if (or
				  (listp x)
				  (is-rest x))
				 0
				 1)) bar)))


(defun create-single-rthm-seqs (rspbars pitchcurves &key (start-id 1))
  (make-rthm-seq (list start-id (append (list cbars)
					`(:pitch-seq-palette
					  (,pitchcurves))))))
(defun create-multiple-rthm-seqs (rspbars pitchcurves time-sig
				  &key (start-id 1))
    (loop for rs in rspbars
	 for pc in pitchcurves
	 for i from start-id
	 collect `(,i ((,(append (list (data time-sig)) rs))
		       :pitch-seq-palette (,pc)))))

(defun create-pc-live-cells (livecells pitch-set)
  (let* ((pc (loop for l in livecells
		collect (remove-duplicates
			 (loop for pc in l
			    for (n 8va) = pc
			    for max8va = (if (< 8va 5) 8va 5)
			    if (< n (1-(length pitch-set)))
			    collect (combine-into-symbol
				     (nth n pitch-set)
				     (+ 2 max8va))))))
	 (no-nil (remove nil pc)))
    (loop for i in pc
       if (> (length i) 3)
       collect i)))

(defun scale-rhythms (factor rhythms)
  (mapcar #'(lambda (y)
	      (if (is-rest y)
		  (list (data y))
		  (data y)))
	  (mapcar #'(lambda (x)
		      (scale x factor))
		  (values-to-rhythms rhythms))))

;; recursive functions
(defmethod m91 ((m math-operations) n)
  (if (> n 100)
      (- n 10)
      (progn
	(setf (m91sub m) (append (m91sub m) (list n)))
	(m91 m (m91 m (+ n 11))))))


