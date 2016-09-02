;; Class that creates and manipulates Elementary CA
;; @author John Palma


(defclass elementary-automata ()())

;; According to the information in rule-array calculates
;; the next generation			
;; values: list of three values corresponding to the cell 
;; and neighbours to compare against the rule array
(defmethod calculate-rule (values (rule-array vector))
  (let* ((val (loop for r in values
		   for ex from (1- (length values)) downto 0
		 sum (* r (expt 2 ex)) ))
	 (index (abs (- val (1- (length rule-array))))))
    (aref rule-array index)))

;; Retrieves the live neighbours for a position i in a vector l
(defmethod getneighbours ((l vector) i)
  (let* ((last (1- (length l)))
	 ;; fix wraparound
	 (indexes (cond ((eq i 0)
			 (list last i (1+ i)))
			((eq i last)
			 (list (1- i) i 0))
			(t (list (1- i) i (1+ i))))))
    (loop for j in indexes
       collect (aref l j))))

;; According to the information in rule-vector calculates
;; the next generation			
;; current: current generation 
(defmethod next-gen ((rule-vector vector) (current vector))
  (let* ((new-gen (make-array (length current)
			      :element-type 'bit
			      :initial-element '0)))
    (loop with len = (length current)
       for i from 1 below (1- len)
       do
	 (setf
	   (bit new-gen i)
	   (calculate-rule
	    (getneighbours current i)
	    rule-vector)))
    new-gen))

;; Retrieves the positions of the live cells in a generation
;; current: current generation
(defmethod get-live-positions ((current vector))
  (loop for i from 1 below (1-(length current))
     for v = (aref current i)
     if (= 1 v)
     collect i))


  
