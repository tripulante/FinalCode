;; Class that creates and manipulates two-dimensional CA
;; Includes rules for Conway's Game of Life, Seeds and Brian's Brain
;; @author John Palma

(defclass automaton ()
  ((grid :accessor grid :initarg nil)))

;; This method creates the grid with the contents of the CA
(defmethod create-grid ((c automaton) width height)
  (setf (grid c) (make-array (list width height)
			     :element-type '(mod 3)
			     :initial-element '0)))
;; For an empty CA, this method initializes cells as alive in a grid.
;; cells is a list of values i,j < (length grid)
;; type in (0, 1, 2) with 0 = Conway, Seeds, Brian's Brain
(defmethod set-live-cells ((c automaton) (cells list) &key (type 0))
  (let* ((live (case type
		 (2 2)
		 (t 1))))
    (loop for i in cells
       do
	 (setf (aref (grid c) (first i) (second i)) live))))

;; Counts the number of live neighbours a cell currently has in an array arr
;; x, y < (length array)
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

;; Decides if a cell is alive or dead in the next generation according
;; to the rules governing the automata
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

;; Creates a new CA generation
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

;; Counts the live cells across rows of a generation
(defmethod count-rows ((c automaton) (arr array))
  (let* ((dim (array-dimensions arr)))
    (loop for i from 0 below (first dim)
       collect (loop for j from 0 below (second dim)
		  sum (aref arr i j)))))
;; Counts the live cells across columns of a generation
(defmethod count-cols ((c automaton) (arr array))
  (let* ((dim (array-dimensions arr)))
    (loop for j from 0 below (second dim)
       collect (loop for i from 0 below (first dim)
		  sum (aref arr i j)))))

;; Gets the total amount of live cells in a generation
(defmethod get-live-cells ((c automaton) (arr array))
  (let* ((dim (array-dimensions arr)))
    (loop for i from 0 below (first dim)
       append (loop for j from 0 below (second dim)
		 for c = (aref arr i j)
		 if (/= 0 c)
		 collect (list i j)))))
;; Does a total sum of the live cells in a generation
(defmethod sum-all-cells ((c automaton) (arr array))
  (loop for i from 0 below (array-total-size arr)
     sum (row-major-aref arr i)))

;; (defmethod create-palette-from-cells ((c automaton)
;; 				      (live list)
;; 				      ))




