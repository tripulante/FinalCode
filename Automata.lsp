(defclass automaton ()
  ((grid :accessor grid :initarg nil)))

(defmethod create-grid ((c automaton) width height &optional contents)
  (setf (grid c) (make-array (list width height)
			     :element-type 'bit
			     :initial-contents (or contents
						   (loop for i from 0 below width
						      collect
							(loop for j from 0 below height
							   for k from (mod i 2)
							   collect (mod k 2)))))))

(defmethod initial-state ((c automaton))
  )

(defmethod check-neighbours ((c automaton) x y (arr array) &key (brain nil))
  (let* ((dimensions (array-dimensions arr))
	 (rows (first dimensions))
	 (cols (second dimensions))
	 (counter 0))
    (loop for i in (list (1- x) x (1+ x))
       do
	 (loop for j in (list (1- y) y (1+ y))
	    unless (and (= i x) (= j y))
	    do
	      (incf counter (if (or (not (< -1 i rows))
				    (not (< -1 j cols)))
				0
				(progn
				  (if brain
				      (progn
					(if (< (aref (grid c) i j) 2)
					    0
					    1))
				      (aref arr i j)))))))
    counter))

(defmethod check-neighbours ((c automaton) x y)
  (let* ((dimensions (array-dimensions (grid c)))
	 (rows (first dimensions))
	 (cols (second dimensions))
	 (counter 0))
    (loop for i in (list (1- x) x (1+ x))
       do
	 (loop for j in (list (1- y) y (1+ y))
	    unless (and (= i x) (= j y))
	    do
	      (incf counter (if (or (not (< -1 i rows))
				    (not (< -1 j cols)))
				0
				(aref (grid c) i j)))))
    counter))

(defmethod check-neighbours-brain ((c automaton) x y)
  (let* ((dimensions (array-dimensions (grid c)))
	 (rows (first dimensions))
	 (cols (second dimensions))
	 (counter 0))
    (loop for i in (list (1- x) x (1+ x))
       do
	 (loop for j in (list (1- y) y (1+ y))
	    unless (and (= i x) (= j y))
	    do
	      (incf counter (if (or (not (< -1 i rows))
				    (not (< -1 j cols)))
				0
				(progn
				  (if (< (aref (grid c) i j) 2)
				      0
				      1))))))
    counter))

(defmethod decide-life ((c automaton) x y (arr array))

  (let* ((neighbours (check-neighbours c x y))
	 (cell (aref (grid c) x y))
	 (alive (cond ((or (and (eq cell 1)
				(<= 2 neighbours 3))
			   (and (eq cell 0)
			  (eq neighbours 3)))
		       1)
		      (t 0))))
    (setf (aref (grid c) x y) alive)
    alive)
  )

(defmethod decide-life-conway ((c automaton) x y)
  (let* ((neighbours (check-neighbours c x y))
	 (cell (aref (grid c) x y))
	 (alive (cond ((or (and (eq cell 1)
				(<= 2 neighbours 3))
			   (and (eq cell 0)
			  (eq neighbours 3)))
		       1)
		      (t 0))))
    (setf (aref (grid c) x y) alive)
    alive))
(defmethod decide-life-seeds ((c automaton) x y)
  (let* ((neighbours (check-neighbours c x y))
	 (cell (aref (grid c) x y))
	 (alive (cond ((and (= cell 0) (= 2 neighbours)) 1)
		      (t 0))))
    (setf (aref (grid c) x y) alive)
    alive))

(defmethod decide-life-brain ((c automaton) x y)
  (let* ((neighbours (check-neighbours-brain c x y))
	 (cell (aref (grid c) x y))
	 (alive (cond ((and (= cell 0) (= 2 neighbours)) 1)
		      (t 0))))
    (setf (aref (grid c) x y) alive)
    alive))

(defmethod conway-step ((c automaton))
  (let* ((dimensions (array-dimensions (grid c)))
	 (results (make-array dimensions
			      :element-type 'bit)))
    (loop for i from 0 below (first dimensions)
       do
	 (loop for j from 0 below (second dimensions)
	    do
	      (setf (aref results i j)
		    (decide-life-conway c i j))))
    ;; (print (grid c))
    results))

(defmethod seeds-step ((c automaton))
  (let* ((dimensions (array-dimensions (grid c)))
	 (results (make-array dimensions
			      :element-type 'bit)))
    (loop for i from 0 below (first dimensions)
       do
	 (loop for j from 0 below (second dimensions)
	    do
	      (setf (aref results i j)
		    (decide-life-seeds c i j))))
    ;; (print (grid c))
    results))

(defmethod brain-step ((c automaton))
  (let* ((dimensions (array-dimensions (grid c)))
	 (results (make-array dimensions
			      :element-type '(mod 3)
			      :initial-element '0)))
    (loop for i from 0 below (first dimensions)
       do
	 (loop for j from 0 below (second dimensions)
	    do (setf (aref results i j)
		     (decide-life-brain c i j))))
    results))

(let* ((auto (make-instance 'automaton))
       (counter 0)
       (dim '(100 100)))
  (create-grid auto 100 100)
  ;; (setf (grid auto) (make-array dim
  ;; 			     :element-type '(mod 3)
  ;; 			     :initial-contents (loop for i from 0 below (first dim)
						    
  ;; 						  collect
  ;; 						    (loop for j from 0 below (second dim)
  ;; 						       for k from (mod i 3) by 2
  ;; 						       collect (mod k 3)
  ;; 						       do
  ;; 							 (incf counter)))))
  ;; (print (grid auto))
  (print (loop for i below 5
  	    collect
  	      (loop for j below 5
  		 collect (check-neighbours-brain auto i j))))
  (print (grid auto))
  ;; (loop repeat 10
  ;;    do (conway-step auto))
  ;; (print "steps")
  (loop repeat 10
     do (print (seeds-step auto)))
  ;; (loop repeat 10
  ;;      do (print (brain-step auto)))
  (print (grid auto)))
