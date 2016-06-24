;; (defclass node ()
;;   ((id :accessor id :initarg :id)))

;; (defclass edge ()
;;   ((weight :accessor weight :initarg 0)
;;    (nodes :accessor nodes :initarg nil)))

(defclass graph ()
  ((vertices :accessor vertices :initarg nil)
   (edges :accessor edges :initarg nil)
   (weighted :accessor weighted :initarg nil)
   (adjmatrix :accessor matrix :initarg nil)
   (infinity :accessor infinity :initform 1000000)
   (explored :accessor explored :initform (list))))



(defmethod create-adjacency-matrix ((graph graph) (numnodes integer) &key (weighted nil))
  (setf (matrix graph) (make-array (list numnodes numnodes)
				   :element-type 'number
				   :initial-element (infinity graph))
	(weighted graph) weighted
	(vertices graph) numnodes))

(defmethod add-am-edge ((graph graph) n1 n2 &key (weight 0))
  (let* ((weighted? (weighted graph))
	 (w (if (null weighted?)
		1
		weight)))
    (if (array-in-bounds-p (matrix graph) n1 n2)
	(progn ; (print w)
	  (setf (aref (matrix graph) n1 n2) w
		(aref (matrix graph) n2 n1) w)))))

(defmethod remove-am-edge ((graph graph) n1 n2)
  (if (array-in-bounds-p (matrix graph) n1 n2)
      (setf (aref (matrix graph) n1 n2) nil
	    (aref (matrix graph) n2 n1) nil)))

(defmethod check-edge ((graph graph) n1 n2)
  (if (array-in-bounds-p (matrix graph) n1 n2)
      (aref (matrix graph) n1 n2)
      nil))

(defmethod adjacent-edges ((graph graph) vertex)
  (if (array-in-bounds-p (matrix graph) 0 vertex)
      (loop for i from 0 below (array-dimension (matrix graph) 0)
	 for v = (aref (matrix graph) vertex i)
	 if (and (not (null v))
		(/= (infinity graph) v))
	 collect i)
      nil))

(defmethod dfs ((graph graph) vertex)
  (setf (explored graph) (append (explored graph) (list vertex)))
  (loop for i in (adjacent-edges graph vertex)
     if (not (position i (explored graph)))
     do
       (print i)
       (dfs graph i)))

(defmethod bfs ((graph graph) vertex)
  (let* ((queue (list))
	 (numvertex (array-dimension (matrix graph) 0))
	 (res (list))
	 (marked (make-array numvertex
			     :element-type 'boolean
			     :initial-element 'nil)))
    
    (setf queue (append queue (list vertex))
	  res (append res (list vertex)))
    (setf (aref marked vertex) t)
    (loop while queue
       for v = (pop queue)
       for adj = (adjacent-edges graph v)
       do
	 (loop for i in adj
	    if (not (aref marked i))
	    do
	      (setf queue (append queue (list i))
		    (aref marked i) t
		    res (append res (list i)))
	      (print marked)))
    res))
;; implementation of Dijkstra's algorithm for a connected, weighted graph
(defmethod dijkstra ((graph graph) startv)
  (let* ((numvertex (vertices graph))
	 (vset (loop for i from 0 below numvertex collect i))
	 (dist (make-array numvertex :initial-element (infinity graph)))
	 (prev (make-array numvertex :initial-element 'nil)))
    (setf (aref dist startv) 0)
    (loop while vset
       for u = (loop for i in vset
    		  with mindist = (infinity graph)
    		  with mindex = -1
    		  if (< (aref dist i) mindist)
    		  do
    		    (setf mindex i)
    		  finally (return mindex))
       for adju = (adjacent-edges graph u)
       do
    	 (setf vset (delete u vset))
    	 (loop for v in adju
    	    for alt = (+ (aref dist u)
    	 		 (aref (matrix graph) u v))
    	    if (< alt (aref dist v))
    	    do
    	      (setf (aref dist v) alt
    	 	    (aref prev v) u)))
    (values dist prev)))
;; implementation of the Minimum Spanning Tree using Prim's Algorithm
;; graph is connected and weighted
(defmethod prims-algorithm ((graph graph) start)
  (let* ((numvertex (vertices graph))
	 (reached (make-array numvertex
			      :element-type 'boolean
			      :initial-element 'nil))
	 
	 (result (make-array numvertex :initial-element '0)))
    (setf (aref reached start) t
	  (aref result start) start)
    (loop repeat (1- numvertex)
       for (x y) = '(0 0)
       do
	 (loop for i from 0 below numvertex
	    do
	      (loop for j from 0 below numvertex
		 for ri = (aref reached i)
		 for rj = (aref reached j)
		 for cij = (aref (matrix graph) i j)
		 for cxy = (aref (matrix graph) x y)
		 if (and ri
			 (not rj)
			 (< cij cxy))
		 do
		   (setf x i
			 y j)))
	 (setf (aref result y) x
	       (aref reached y) t)
	 (print (list x y)))
    result))


