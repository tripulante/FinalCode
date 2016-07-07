
;; calculates both adiabatic and isothermal bubble-pulsation
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
(defun scale-value (value oldMin oldMax newMin newMax)
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



;; Calculates n value sets of a Lorenz System, given values for
;; sigma, beta and ro
(defmethod lorenz-system (n sigma beta ro &key (init 0.01))
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
;; Calculates n value sets of a Rössler Attractor, given values for
;; a, b and c
(defmethod rossler-attractor (n a b c &key (inc 0.01) (x1 1.0) (y1 1.0) (z1 1.0))
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

;; Calculates a set of values for a double pendulum given the masses
;; of each mass, the length of each pendulum, the initial angles
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

;; calculates the total time elapsed (in minutes) given a number of measures,
;; a beat unit (e.g. if in 6/8 time then the beat unit is 2 dotted quarters)
;; and a tempo value
(defun calculate-total-time (measures beatunit tempo)
  (/ (* measures beatunit) tempo))

;; Calculates the total set of measures required to reach
;; a certain time limit (in minutes). Requires also a
;; beat unit and a tempo value
(defun calculate-measures (total tempo beatunit)
  (/ (* total tempo) beatunit))


;; Calculates an approximate beat unit given a tempo value,
;; a certain time limit (in minutes) and a number of measures
(defun calculate-beat-unit (total measures tempo)
  (/ (* total tempo) measures))
;; Calculates an approximate tempo
;; given a certain time limit (in minutes), a
;; beat unit and a measure number
(defun calculate-tempo (total measures beatunit)
  (/ (* measures beatunit) total))

;; create a bar from a set of single durations
;; fill the empty spaces with a single note
;; values greater than bar length return an empty bar
;; Returns a set of bars formatted for a single rsp,
;; the individual bars and a list with each bar
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
					(if (/= index i)
					    (setf index (1- i)))
					))))
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
	 (databars (loop for i in complete-bars
		      collect (mapcar #'(lambda (x)
					  (if (is-rest x)
					      (list (data x))
					      (data x))) i)))
	 (firstbar (append (list (data timesig))
			   (first databars)))
	 (rspbar (append (list firstbar)
			 (rest databars)))
	 )
    ;; (print rspbar)
    (values rspbar complete-bars databars)))

;; A method that generates a rhythm hash table when needed
(defun rhythm-hash (keys rhythms)
  (if (= (length keys) (length rhythms))
      (let* ((mymap (make-hash-table :size (length keys))))
	(loop for k in keys
	   for r in rhythms
	   do
	     (setf (gethash k mymap) r))
	mymap)
      nil))

;; Converts a list of rhythmic values (e.g. '(s q (h))) to a list of
;; SC Rhythm objects
(defun values-to-rhythms (rthm-values)
  (map 'list #'(lambda (x) (if (or (symbolp x)
				   (numberp x))
			       (make-rhythm x)
			       (make-rhythm (pop x) :is-rest t))) rthm-values))

;; Given a bar of SC rhythm objects, counts how many rhythms are actual attacks
(defun get-attacks (bar)
  (reduce #'+ (mapcar
	       #'(lambda (x) (if (or
				  (listp x)
				  (is-rest x))
				 0
				 1)) bar)))

;; Given a list of bars and an equal number of pitchcurves creates a single
;; rhythm sequence map
(defun create-single-rthm-seqs (rspbars pitchcurves &key (start-id 1))
  (make-rthm-seq (list start-id (append (list rspbars)
					`(:pitch-seq-palette
					  (,pitchcurves))))))

;; Given a time signature, a list of bars and a list of pitchcurves
;; creates one rhythm sequence per bar
(defun create-multiple-rthm-seqs (rspbars pitchcurves time-sig
				  &key (start-id 1))
    (loop for rs in rspbars
	 for pc in pitchcurves
	 for i from start-id
	 collect `(,i ((,(append (list (data time-sig)) rs))
		       :pitch-seq-palette (,pc)))))

;; Given a list of live cells (i.e. a two integer pair) and a set of starting
;; pitches (e.g. '(c e g)) creates several pitch sets 
(defun create-pc-live-cells (livecells pitch-set)
  (let* ((pc (loop for l in livecells
		collect (remove-duplicates
			 (loop for pc in l
			    for (n 8va) = pc
			    for pos = (mod n (length pitch-set))
			    collect (combine-into-symbol
				     (nth pos pitch-set)
				     (+ 2 (mod 8va 6)))))))
	 (no-nil (remove nil pc)))
    (loop for i in no-nil
       if (> (length i) 3)
       collect i)))

;; Scales a list of rhythms by the given factor
(defun scale-rhythms (factor rhythms)
  (mapcar #'(lambda (y)
	      (if (is-rest y)
		  (list (data y))
		  (data y)))
	  (mapcar #'(lambda (x)
		      (scale x factor))
		  (values-to-rhythms rhythms))))

;; Creates a dragon curve fractal and uses the data to generate a
;; pitch set
;; Variables: X, Y
;; Constants: F, -, +
;; Axiom: 
(defun dragon-to-ps (startnote numreps &key (offset '(0 0 0)))
  (let*
    ((dragon (make-l-for-lookup 'dragon

				'((1 ((1)))
				  (2 ((2)))
				  (3 ((3)))
				  (4 ((4)))
				  (5 ((5))))
				'((1 (1 4 2 3 4))
				  (2 (5 3 1 5 2))
				  (3 (3))
				  (4 (4))
				  (5 (5)))))
     (pitchset (loop for dr in (get-l-sequence dragon 1 numreps)
		  for start = (note-to-midi startnote)
		  with (l r f) = offset
		  for current = start then
		    (case dr
		      (3 (+ current 2 f))
		      (4 (+ current dr r))
		      (5 (- current dr l))
		      (t current))
		  collect (midi-to-note
			   (cond ((>= current 144) 143)
				 ((< current 0) 0)
				 (t current))))))
    (values (remove-duplicates pitchset) pitchset)))

;; Creates a dragon curve fractal and uses the data to generate a
;; pitch set
;; Variables: X, Y
;; Constants: F, -, +
;; Axiom: 
(defun dragon-curve (startnote numreps)
  (let*
    ((dragon (make-l-for-lookup 'dragon

				'((1 ((1)))
				  (2 ((2)))
				  (3 ((3)))
				  (4 ((4)))
				  (5 ((5))))
				'((1 (1 4 2 3 4))
				  (2 (5 3 1 5 2))
				  (3 (3))
				  (4 (4))
				  (5 (5)))))
     (pitchset (loop for dr in (get-l-sequence dragon 1 numreps)
		  for start = (note-to-midi startnote)
		  for current = start then
		    (case dr
		      (3 (+ current 2))
		      (4 (+ current dr))
		      (5 (- current dr 2))
		      (t current))
		  collect current))
     (no-dups (remove-duplicates pitchset)))
    (values no-dups pitchset)))
;; Creates a Koch Snowflake curve and uses the data to generate a
;; pitch set
;; Variables: X, Y
;; Constants: F, -, +
;; Axiom:
;; '((1 ((f)))
;;   (2 ((+)))
;;   (3 ((-))))
(defun koch-to-ps (startnote numreps &key
				       (type2 nil)
				       (offset '(0 0 0)))
  (let*
      ((koch (make-l-for-lookup 'koch
				'((1 ((1)))
				  (2 ((2)))
				  (3 ((3))))
				'((1 (1 2 1 3 1 3 1 2 1))
				  (2 (2))
				  (3 (3)))))
       (koch2 (make-l-for-lookup 'koch
			       '((1 ((1)))
				 (2 ((2)))
				 (3 ((3))))
			       '((1 (1 3 1 2 2 1 3 1))
				 (2 (2))
				 (3 (3)))))
       (koch-sequence (cond (type2 (get-l-sequence koch 1 numreps))
			    (t (get-l-sequence koch2 1 numreps))))
       (offset-l (first offset))
       (offset-r (second offset))
       (offset-f (third offset))
       (pitchset (loop for kh in koch-sequence
		    for start = (note-to-midi startnote)
		    for current = start then
		      (case kh
			(2 (+ current kh offset-l))
			(3 (- current kh offset-r))
			(t (+ current offset-f)))
		    collect (midi-to-note
			     (cond ((>= current 144) 143)
				   ((< current 0) 0)
				   (t current)))))
       (no-duplications (remove-duplicates pitchset)))
    (values no-duplications pitchset)))

;; Creates a Koch Snowflake curve and uses the data to generate a
;; pitch curve
;; Variables: X, Y
;; Constants: F, -, +
;; Axiom:
;; '((1 ((f)))
;;   (2 ((+)))
;;   (3 ((-))))
(defun koch-pitch-curve (startvalue numreps &key (maxvalue 8)
					      (type2 nil)
					      (offset '(0 0 0))
					      )
  (let*
      ((koch (make-l-for-lookup 'koch
				'((1 ((1)))
				  (2 ((2)))
				  (3 ((3))))
				'((1 (1 2 1 3 1 3 1 2 1))
				  (2 (2))
				  (3 (3)))))
       (koch2 (make-l-for-lookup 'koch
			       '((1 ((1)))
				 (2 ((2)))
				 (3 ((3))))
			       '((1 (1 3 1 2 2 1 3 1))
				 (2 (2))
				 (3 (3)))))
       (koch-sequence (cond (type2 (get-l-sequence koch 1 numreps))
			    (t (get-l-sequence koch2 1 numreps))))
       (offset-l (first offset))
       (offset-r (second offset))
       (offset-f (third offset)))
    (loop for kh in koch-sequence
       for current = startvalue then
	 (case kh
	   (2 (+ current kh offset-l))
	   (3 (- current kh offset-r))
	   (t (+ current offset-f)))
       collect (mod current maxvalue))))

;; Creates a Customised Chord Function
(defun second-chord (curve-num index pitch-list pitch-seq instrument set)
  (chord-fun-aux curve-num index pitch-list pitch-seq instrument set 4 3 14))

