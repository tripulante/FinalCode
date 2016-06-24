(defclass automaton ()
  ((grid :accessor grid :initarg nil)))

(defmethod create-grid ((c automaton) width height &optional contents
			&key (empty nil))
  (setf (grid c) (make-array (list width height)
			     :element-type '(mod 3)
			     :initial-element '0
			     )))

(defmethod set-live-cells ((c automaton) (cells list) &key (type 0))
  (let* ((live (case type
		 (2 2)
		 (t 1))))
    (loop for i in cells
       do
	 (setf (aref (grid c) (first i) (second i)) live))))

(defmethod check-neighbours ((c automaton) x y (arr array) &key (type 0))
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
				(case type
				  (2 (if (< (aref arr i j) 2)
					    0
					    1))
				  (t (aref arr i j)))))))
    counter))


(defmethod decide-life ((c automaton) x y (arr array) &key (type 0))
  (let* ((neighbours (check-neighbours c x y arr :type type))
	 (cell (aref arr x y))
	 (alive (case type
		  (1 (cond ((and (= cell 0) (= 2 neighbours)) 1)
			   (t 0)))
		  (2 (cond ((and (= cell 0) (= 2 neighbours)) 2)
			   ((= cell 2) 1)
			   (t 0)))
		  (t (cond ((or (and (eq cell 1)
				     (<= 2 neighbours 3))
				(and (eq cell 0)
				     (eq neighbours 3)))
			    1)
			   (t 0))))))
    alive))

(defmethod castep ((c automaton) (arr array) &key (type 0))
  (let* ((dimensions (array-dimensions arr))
	 (results (make-array dimensions
			      :element-type '(mod 3)
			      :initial-element '0)))
    (loop for i from 0 below (first dimensions)
       do
	 (loop for j from 0 below (second dimensions)
	    do (setf (aref results i j)
		     (decide-life c i j arr :type type))))
    results))

(defmethod count-rows ((c automaton) (arr array))
  (let* ((dim (array-dimensions arr)))
    (loop for i from 0 below (first dim)
       collect (loop for j from 0 below (second dim)
		  sum (aref arr i j)))))

(defmethod count-cols ((c automaton) (arr array))
  (let* ((dim (array-dimensions arr)))
    (loop for j from 0 below (second dim)
       collect (loop for i from 0 below (first dim)
		  sum (aref arr i j)))))

(defmethod get-live-cells ((c automaton) (arr array) &key (type 0))
  (let* ((dim (array-dimensions arr)))
    (loop for i from 0 below (first dim)
       append (loop for j from 0 below (second dim)
		 for c = (aref arr i j)
		 if (/= 0 c)
		 collect (list i j)))))

(defmethod sum-all-cells ((c automaton) (arr array))
  (loop for i from 0 below (array-total-size arr)
     sum (row-major-aref arr i)))

(defmethod create-palette-from-cells ((c automaton)
				      (live list)
				      ))


;; (let* ((auto (make-instance 'automaton))
;;        (counter 0)
;;        (dim '(100 100))
;;        (type 0)
;;        (results nil)
;;        (noteset nil))
;;   (create-grid auto 100 100)
  
;;   (setf results (loop repeat 10
;; 		   for arr = (grid auto)
;; 		   then (castep auto arr :type type)
;; 		   collect arr))
;;   (setf noteset (loop for r in results
;; 		   collect
;; 		     (loop for i from 0 below 100
;; 			collect (+ 10
;; 			  (loop for j from 0 below 100
;; 			       for v = (aref r j i)
;; 			     sum v)))))
;;   ;; (print results)
;;   (print noteset)
;;   (print (loop for i in noteset
;; 	      for j from 1
;; 	      collect (list j (list (mapcar #'midi-to-note i)))))
;;   )

(let* ((auto (make-instance 'automaton))
       (counter 0)
       (dim '(12 5))
       (type 0)
       (results nil)
       (noteset '(a af b bf c cs d ds e ef f g))
       (livecells nil)
       (pitchsets nil))
  (create-grid auto 12 7 nil :empty t)
  
  ;; (live-cells auto '((5 1) (5 2) (6 1) (6 2) (5 11) (6 11) (7 11) (4 12)
  ;; 		     (3 13) (3 14) (8 12) (9 13) (9 14) (6 15) (4 16)
  ;; 		     (5 17) (6 17) (7 17) (6 18) (8 16) (3 21) (4 21)
  ;; 		     (5 21) (3 22) (4 22) (5 22) (2 23) (6 23) (1 25)
  ;; 		     (2 25) (6 25) (7 25) (3 35) (4 35) (3 36) (4 36)))
  ;; beacon (1 1) (2 1) (1 2) (4 3) (3 4) (4 4)
  ;; glider (1 0) (2 1) (0 2) (1 2) (2 2)
  (set-live-cells auto '((1 0) (2 1) (0 2) (1 2) (2 2) (1 1) (1 2) (4 3) (3 4) (4 4)) :type type) 
  (setf results (loop repeat 20
		   for arr = (grid auto)
		   then (castep auto arr :type type)
		   collect arr))
  (setf livecells (loop for r in results
		     collect (get-live-cells auto r)))
  (setf pitchsets (loop for l in livecells
	    collect (remove-duplicates
		     (loop for pc in l
		       for (n 8va) = pc
		       collect (combine-into-symbol
				(nth n noteset)
				(1+ 8va))))))
  (print pitchsets)
  (print (loop for i in (mapcar #'(lambda (x)
				    (data (make-tl-set x :transposition 2))) pitchsets)
	      collect (mapcar #'data i)))
  ;; (with-open-file (stream "~/Documents/uni/name.txt" :direction :output :if-exists :supersede)
  ;;   (print results stream))
  ;; (print (length results))
  ;; (print (loop for r in results
  ;; 	    collect (get-live-cells auto r)))
  ;; (print (loop for r in results
  ;; 	    for rs = (count-rows auto r)
  ;; 	    for cs = (count-cols auto r)
  ;; 	    collect (list rs cs)))
  ;; (print (loop for r in results
  ;; 	    collect (sum-all-cells auto r)))
  ;; (print (loop for r in results
  ;; 	    for l = (get-live-cells auto r)
  ;; 	    collect (loop for j in l
  ;; 			 collect (make-rhythm (second j)))))
  )

;; (let* ((motif '(e e q q))
;;        (rhythms (append (loop repeat 3 append motif) '(h.)))
;;        (notes '(e6 d5 fs5 gs5 cs6 b5 d5 e5 b5 a5 cs5 e5 a5))
;;        (events (loop for r in rhythms and n in notes collect
;;                     (make-event n r))))
;;   ;; (events-update-time events)
;;   (event-list-to-midi-file events :start-tempo 150))

