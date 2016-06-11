(let* ((koch1 (make-l-for-lookup 'koch
				'((1 ((f)))
				  (2 ((+)))
				  (3 ((-))))
				'((1 (1 2 1 3 1 3 1 2 1) )
				  (2 (2))
				  (3 (3)))))
       (koch2 (make-l-for-lookup 'koch
				 '((1 ((f)))
				   (2 ((+)))
				   (3 ((-))))
				 '((1 (1 3 1 2 2 1 3 1) )
				   (2 (2))
				   (3 (3)))))
       (dragon (make-l-for-lookup 'dragon
				  '((1 ((x)))
				    (2 ((y)))
				    (3 ((f)))
				    (4 ((+)))
				    (5 ((-))))
				  '((1 (1 4 2 3 4))
				    (2 (5 3 1 5 2))
				    (3 (3))
				    (4 (4))
				    (5 (5))))))
  (print (do-lookup koch1 1 57))
  (print (do-lookup koch2 1 57))
  (print (do-lookup dragon 1 100)))

(let* ((levels 5)
       (value 10000)
       (values '(a b c))
       (segs 3))
  (defun sschop (value segments levels)
    (if (< levels 1)
	'()
	(let* ((nextval (floor value segments))
	       (chopped (loop repeat segments
			   append (sschop nextval segments (1- levels)))))
	  (list* value chopped))))
  (defun sierpinski (values levels dur)
    (let* ((len (length values))
	   (nval (loop for i in values
		      append (list* i dur))))
      (if (> levels 1)
	  (list* nval (sierpinski values (1- levels) (/ dur len)))
	  '())))
  (print (sschop value segs levels))
  (print (sierpinski values levels 50.0))
  )
