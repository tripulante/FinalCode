(defclass sorting-class ()
    ((subsorts :accessor subsorts :initform nil)))

(defmethod merge-sort ((sc sorting-class) lst)
  (let ((size (length lst)))
    (if (= size 1)
	lst
	(progn
	  (let* ((seq1 (merge-sort sc (subseq lst 0 (floor (/ size 2)))))
		 (seq2 (merge-sort sc (subseq lst (floor (/ size 2)) size)))
		 (merged (merge 'list seq1 seq2 #'<)))
	    (setf (subsorts sc) (append (subsorts sc) (list (copy-list merged))))
	    merged)))))

(defmethod partition ((sc sorting-class) l lo hi)
  (let* ((i lo))
    (loop with pivot = (nth hi l)
       for j from lo to (1- hi)
       if (<= (nth j l) pivot)
       do
	 (rotatef (nth i l) (nth j l))
	 (setf i (1+ i)))
    (rotatef (nth i l) (nth hi l))
    i))
(defmethod quick-sort ((sc sorting-class) l lo hi)
  (cond ((< lo hi)
	 (let ((p (partition sc l lo hi)))
	   (setf (subsorts sc) (append (subsorts sc) (list (copy-list l))))
	   (quick-sort sc l lo (1- p))
	   (quick-sort sc l (1+ p) hi)))))


(defmethod insertion-sort ((sc sorting-class) l)
  (setf (subsorts sc) 
	(loop for i below (length l)
	   append 
	     (loop for j from i downto 1
		while (> (nth (1- j) l) (nth j l))
		collect (copy-list l)
		do
		  (rotatef (nth j l) (nth (1- j) l))))))


(defmethod clear-stored-sort ((sc sorting-class))
  (setf (subsorts sc) nil))

(let* ((sc (make-instance 'sorting-class))
       (l '(3 7 4 9 5 2 6 1)))
  (insertion-sort sc (copy-list l))
  (print (subsorts sc))
  (clear-stored-sort sc)
  (merge-sort sc l)
  (print (subsorts sc))
  (clear-stored-sort sc)
  (quick-sort sc (copy-list l) 0 (1- (length l)))
  (print (subsorts sc)))
