;; this function implements a logistic map
;; https://en.wikipedia.org/wiki/Logistic_map
(defun logistic-map (x r)
  (* x r (- 1 x)))
;; this function scales values like the Max object scale
(defun scale-value (value oldMin oldMax newMin newMax)
  (+ (/ (* (- value oldMin)
	   (- newMax newMin)) (- oldMax oldMin)) newMin))
;; Runs the logistic map n times, starting from an initial x0
;; value and then scales it to (min max) values
;; no error checking
(defun logistic-curve (x0 r n min max)
  (loop repeat n
     for x = (logistic-map x0 r)
     then (logistic-map x r)
     collect (round (scale-value x 0 1 min max))))


;; Sorting functions


(let* ((x 10.5))
  (print (- x)))


(defun merge (l1 l2)
  (setf final (loop for i in l1
		 for j in l2
		 until (or (null i) (null j))
		 if (> i j)
		 collect j
		 else
		 collect i))
  (cond ((length l1) > (length l2)))
    
    
  )
  (defun mergesort (l begin end)
    (let ((size (length l)))
      )
    (cond ((< (- end begin) 2) l)
	  (t (setf middle (floor (/ (+ begin end) 2)))
	     (setf subs (split-into-sub-groups3 l middle))
	     (setf lh (mergesort (first subs) begin middle))
	     (setf rh (mergesort (second subs) (1+ middle) end))
	     (setf e (merge lh rh))
	     (print e)))
    )





(let* ((l '(3 7 4 9 5 2 6 1))
       (res nil))
  (defun partition (l lo hi)
    (setf i lo)
    (loop with pivot = (nth hi l)
       for j from lo to (1- hi)
       if (<= (nth j l) pivot)
       do
	 (rotatef (nth i l) (nth j l))
	 (setf i (1+ i)))
    (rotatef (nth i l) (nth hi l))
    (print p)
    (return-from partition i))
  (defun quick-sort (l lo hi)
	(cond ((< lo hi)
	       (print l) ;; collection point
	       (setq p (partition l lo hi))
	       (quick-sort l lo (1- p))
	       (quick-sort l (1+ p) hi))))
    (defun merge-it (l1 l2)
      (let ((new-arr (make-array (+ (length l1)
				  (length l2))
				 :fill-pointer 0)))
	(loop for idx from 0 to (+ (length l1) (length l2)) do
	     (let ((x (car l1))
		   (y (car l2)))
	       (when (and (not (null x)) (not (null y)))
		 (if (<= x y)
		     (progn
		       (setf l1 (cdr l1))
		       (vector-push x new-arr))
		     (progn
		       (setf l2 (cdr l2))
		       (vector-push y new-arr))))))
	(mapcar #'(lambda (e) (vector-push e new-arr)) (append l1 l2))
	(coerce new-arr 'list)))
    (defun merge-sort-aux (lst results)
      (let ((size (length lst)))
	(if (= size 1)
	    lst
	    (progn
	      (let* ((seq1 (merge-sort-aux (subseq lst 0 (floor (/ size 2))) results))
		     (seq2 (merge-sort-aux (subseq lst (floor (/ size 2)) size) results))
		     (merged (merge 'list seq1 seq2 #'<)))
		(print merged)
		(setf results (append (copy-list results) (copy-list merged)))
		merged ))))
	   )
    (defun merge-sort (lst)
      (let* ((res nil)
	     (final (merge-sort-aux lst res)))
	
	(values res final)))



  
  ;; (quick-sort l 0 (1- (length l)))     
  ;; (print (merge-sort l 0 (1- (length l))))
  ;; (multiple-value-bind (l res) (merge-sort l))
  (print (multiple-value-list (merge-sort l)))
  ;; (trace merge-sort)
  ;; (print (bubble-sort l))
  (print l)
  ;; (print res)
  )


(let* ((l '(3 7 4 9 5 2 6 1)))
  (defun insertion-sort (l)
	 (loop for i below (length l)
	    append
	      (loop for j from i downto 1
		 while (> (nth (1- j) l) (nth j l))
		 collect (copy-list l)
		 do
		   (rotatef (nth j l) (nth (1- j) l)))))
  (defun bubble-sort (l)
    (let ((results nil))
      (loop with n = (length l)
	 for i from 2 to n
	 with swaps = nil	 
	 do
	   (loop for j from 0 to (- n 2)
	      if (> (nth j l) (nth (1+ j) l))
	      do
		(rotatef (nth j l) (nth (1+ j) l))
		(setf swaps t)
		(setf results (append results (list (copy-list l)))))
	 never (not swaps))
      results))
  ;; (print (merge-sort l 0 (1- (length l))))
  ;; (multiple-value-bind (l res) (merge-sort l))
  ;; (print (multiple-value-list (merge-sort l res)))
  ;; (trace merge-sort)
  ;; (print (bubble-sort l))
  (print l)
  )

(let* ((test 99)
       (width 5)
       (height 5)
       (testa (make-array (list width height)
			  :initial-contents (loop for i from 0 below width
					       for counter from 0
					       collect
						 (loop for j from 0 below height
						    for k from (mod i 2)
						    collect (mod k 2))))))
  ;; (defun m91 (n)
  ;;   (if (> n 100)
  ;; 	(- n 10)
  ;; 	(progn (m91 (m91 (+ n 11))))))
  ;; (trace m91)
  ;; (print (m91 test))
  ;; (print (loop for i from 0 below width
  ;; 	    for counter from 0
  ;; 	    collect
  ;; 	      (loop for j from 0 below height
  ;; 		 collect (mod counter 2))))
  (print (loop for i below width
	    do
	      ;; (print (aref testa i))
	      (loop for j below height
		 do
		   (print (list i j (aref testa i j))))))
  (print testa)
  )


