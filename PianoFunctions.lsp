
;; Creates a tailored instance of a sc piece for piano using CA and
;; the helper functions created before (fractal pitch sets)
(defun piano1-scaled
    (storepath startpitch tempo
     tmin
     rhythms
     rhythms-lh
     time-sig
     &key (scale1 2)
       (scale2 2)
       (usechord nil)
       (palette-type 2)
       (reverse-pc nil)
       (offset '(0 0 2))
       (type 1)
       (used-notes nil)
       (base-unit 'q.)
       (base-tempo-value 2))
  (let*
    ((chord-fn (chord-function
		(get-data 'piano
			  +slippery-chicken-standard-instrument-palette+)))
     (rev (format nil "~A-~A-~A~A"
		  (case palette-type
		    (0 "Koch1")
		    (1 "Koch2")
		    (t "Dragon"))
		  (if usechord
		      chord-fn
		      "")
		  startpitch
		  (if reverse-pc
		      "-R"
		      "")))
     (required-bars (calculate-measures tmin tempo base-tempo-value))
     (auto (make-instance 'automaton))
     (s1-rh (scale-rhythms scale1 rhythms))
     (s2-rh (scale-rhythms scale2 rhythms-lh))
     (generations (progn
		    (create-grid auto (length rhythms) (length rhythms))
		    (set-live-cells auto '((1 0) (2 1) (0 2) (1 2)
					   (2 2) (1 1) (1 2) (4 3)
					   (3 4) (4 4)) :type type)
		    (loop repeat 20 ;100
		       for arr = (grid auto)
		       then (castep auto arr :type type)
		       collect arr)))
     (livecells (loop for g in generations
		   collect (get-live-cells auto g)))
     (lh-live (loop for l in livecells
		 append (mapcar #'first l)))
     (rh-live (loop for l in livecells
		 append (mapcar #'second l)))
     (lh-rhythms (loop for r in rh-live
		    collect (nth r s1-rh)))
     (rh-rhythms (loop for r in rh-live
		    collect (nth r s2-rh)))
     (bar-set (list (multiple-value-list
		     (create-bars (values-to-rhythms lh-rhythms)
				  time-sig
				  :sequential t))
		    (multiple-value-list
		     (create-bars (values-to-rhythms rh-rhythms)
				  time-sig
				  :sequential t))))
     (bars (append (first (mapcar #'third bar-set))
		   (second (mapcar #'third bar-set))))
     (attacklist (mapcar #'get-attacks
			 (append
			  (first (mapcar #'second bar-set))
			  (second (mapcar #'second bar-set)))))
     (pitchcurves (mapcar #'(lambda (x)
			      (if reverse-pc
				  (loop repeat x
				     for i from (1+ x) downto 1
				     collect (if usechord
						 (list i)
						 i))
				  (loop repeat x
				     for i from 1
				     collect (if usechord
						 (list i)
						 i)))) attacklist))
     (rseqs (create-multiple-rthm-seqs bars pitchcurves time-sig))
     (totalbars (length bars))
     (offset-lists (loop repeat 10
		     for v from 0
		     collect (mapcar #'(lambda (x)
					 (+ x v)) offset)))
     (set-offsets (loop for i in offset-lists
		     collect (case palette-type
			       (0 (koch-to-ps startpitch 10 :offset i))
			       (1 (koch-to-ps startpitch 10 :type2 t :offset i))
			       (t (dragon-to-ps startpitch 10 :offset i)))))
     (k-palette (loop for p in set-offsets
		   for i from 1
		   collect (list i (list p))))
     (maplist (loop repeat required-bars
		   for i from 1
		   collect (1+ (mod i (length k-palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "Superposiciones Piano I"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))))
		:tempo-map `((1 (,base-unit ,tempo)))
		:set-palette k-palette
		:set-map `((1 ,maplist))
		:rthm-seq-palette rseqs
		:rthm-seq-map `((1 ((pr ,(loop repeat (length maplist)
					    for i from 1
					    collect (1+ (mod i totalbars))))
				    (pl ,(loop repeat (length maplist)
				    	    for i from (floor totalbars 2)
				    	    collect (1+ (mod i totalbars)))))))
		:avoid-used-notes used-notes)))
  (midi-play template :midi-file (concatenate
			 'string storepath
			 (format nil "P1_~A_~D_~A_~D_scaled_~A.mid"
				 rev
				 tempo
				 (data time-sig)
				 type
				 (list scale1 scale2))))))

(defun piano1-regular
    (storepath startpitch tempo
     tmin
     rhythms
     rhythms-lh
     time-sig
     &key (scale1 2)
       (scale2 2)
       (usechord nil)
       (palette-type 2)
       (reverse-pc nil)
       (offset '(0 0 2))
       (type 1)
       (used-notes nil)
       (base-unit 'q.)
       (base-tempo-value 2))
  (let*
    ((chord-fn (chord-function
		(get-data 'piano
			  +slippery-chicken-standard-instrument-palette+)))
     (rev (format nil "~A-~A-~A~A"
		  (case palette-type
		    (0 "Koch1")
		    (1 "Koch2")
		    (t "Dragon"))
		  (if usechord
		      chord-fn
		      "")
		  startpitch
		  (if reverse-pc
		      "-R"
		      "")))
     (required-bars (calculate-measures tmin tempo base-tempo-value))

     (auto (make-instance 'automaton))
     (generations (progn
		    (create-grid auto (length rhythms) (length rhythms))
		    (set-live-cells auto '((1 1) (2 1) (1 2) (4 3) (3 4) (4 4)
					   (4 0) (6 1) (4 2) (4 2)
					   (6 2) (5 1) (5 2) (8 3)
					   (7 4) (7 4)) :type type)
		    (loop repeat 20 ;100
		       for arr = (grid auto)
		       then (castep auto arr :type type)
		       collect arr)))
     (livecells (loop for g in generations
		   collect (get-live-cells auto g)))
     (lh-live (loop for l in livecells
		 append (mapcar #'first l)))
     (rh-live (loop for l in livecells
		 append (mapcar #'second l)))
     (lh-rhythms (loop for r in rh-live
		    collect (nth r rhythms)))
     (rh-rhythms (loop for r in rh-live
		    collect (nth r rhythms-lh)))
     (sequential t)
     (bar-set (list (multiple-value-list
		     (create-bars (values-to-rhythms lh-rhythms)
				  time-sig
				  :sequential sequential))
		    (multiple-value-list
		     (create-bars (values-to-rhythms rh-rhythms)
				  time-sig
				  :sequential sequential))))
     (bars (append (first (mapcar #'third bar-set))
		   (second (mapcar #'third bar-set))))
     (attacklist (mapcar #'get-attacks
			 (append
			  (first (mapcar #'second bar-set))
			  (second (mapcar #'second bar-set)))))
     (pitchcurves (mapcar #'(lambda (x)
			      (if reverse-pc
				  (loop repeat x
				     for i from (1+ x) downto 1
				     collect (if usechord
						 (list i)
						 i))
				  (loop repeat x
				     for i from 1
				     collect (if usechord
						 (list i)
						 i)))) attacklist))
     (rseqs (create-multiple-rthm-seqs bars pitchcurves time-sig))
     (totalbars (length bars))
     (offset-lists (loop repeat 10
		     for v from 0
		     collect (mapcar #'(lambda (x)
					 (+ x v)) offset)))
     (set-offsets (loop for i in offset-lists
		     collect (case palette-type
			       (0 (koch-to-ps startpitch 10 :offset i))
			       (1 (koch-to-ps startpitch 10 :type2 t :offset i))
			       (t (dragon-to-ps startpitch 10 :offset i)))))
     (k-palette (loop for p in set-offsets
		   for i from 1
		   collect (list i (list p))))
     (maplist (loop repeat required-bars
		   for i from 1
		   collect (1+ (mod i (length k-palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "Superposiciones Piano I"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))))
		:tempo-map `((1 (,base-unit ,tempo)))
		:set-palette k-palette
		:set-map `((1 ,maplist))
		:rthm-seq-palette rseqs
		:rthm-seq-map `((1 ((pr ,(loop repeat (length maplist)
					    for i from 1
					    collect (1+ (mod i totalbars))))
				    (pl ,(loop repeat (length maplist)
				    	    for i from (floor totalbars 2)
				    	    collect (1+ (mod i totalbars)))))))
		:avoid-used-notes used-notes)))
  (midi-play template
	     :midi-file (concatenate
			 'string storepath
			 (format nil "P1_~A_~D_~A_~D_~A.mid"
				 rev
				 tempo
				 (data time-sig)
				 type
				 sequential)))))


(defun piano2
    (storepath startpitch tempo
     tmin
     base-rhythms
     time-sig
     &key (scale1 2)
       (scale2 2)
       (usechord nil)
       (palette-type 2)
       (reverse-pc nil)
       (offset '(0 0 2))
       (used-notes nil)
       (base-unit 'q.)
       (base-tempo-value 2)
       (rule 90))
(let*
    ((chord-fn (chord-function
		(get-data 'piano
			  +slippery-chicken-standard-instrument-palette+)))
     (rev (format nil "~A-~A-~A~A"
		  (case palette-type
		    (0 "Koch1")
		    (1 "Koch2")
		    (t "Dragon"))
		  (if usechord
		      chord-fn
		      "")
		  startpitch
		  (if reverse-pc
		      "-R"
		      "")))
     (rulelist '((90 #*01011010)
		 (30 #*00011110)
		 (60 #*00111100)
		 (62 #*00111110)
		 (126 #*01111110)
		 (99 #*01100011)))
     (rulehash (let ((hash (make-hash-table :size (length rulelist))))
		 (loop for i in rulelist
		    do
		      (setf (gethash (first i) hash) (second i)))
		 hash))
     (rule-vector (gethash rule rulehash))
     (current #*00000100000)
     (required-bars (calculate-measures tmin tempo base-tempo-value))
     (rhythms (scale-rhythms scale1 base-rhythms))
     (rhythms-l (scale-rhythms scale2 base-rhythms))
     (rhash (rhythm-hash
	       (loop for i from 1 below (1- (length current))
		  collect i)
	       rhythms))
     (rhash-l (rhythm-hash
	       (loop for i from 1 below (1- (length current))
		  collect i)
	       rhythms-l))
     (generations (loop repeat 30
		     ;; for i from 0
		     for v = current then (next-gen rule-vector v)
		     collect v))
     (rthm-pattern (loop for i in (mapcar #'get-live-positions generations)
			append (loop for val in i
				  collect (gethash val rhash))))
     (rthm-pattern-l (loop for i in (mapcar #'get-live-positions generations)
			append (loop for val in i
				  collect (gethash val rhash-l))))

     (bar-set (list (multiple-value-list
		     (create-bars (values-to-rhythms rthm-pattern)
				  time-sig
				  :sequential t))
		    (multiple-value-list
		     (create-bars (values-to-rhythms rthm-pattern-l)
				  time-sig
				  :sequential t))))
     (bars (append (first (mapcar #'third bar-set))
		   (second (mapcar #'third bar-set))))
     (totalbars (length bars))
     (attacklist (mapcar #'get-attacks
			 (append
			  (first (mapcar #'second bar-set))
			  (second (mapcar #'second bar-set)))))
     (pitchcurves (mapcar #'(lambda (x)
			      (if reverse-pc
				  (loop repeat x
				     for i from (1+ x) downto 1
				     collect (if usechord
						 (list i)
						 i))
				  (loop repeat x
				     for i from 1
				     collect (if usechord
						 (list i)
						 i)))) attacklist))
     (rseqs (create-multiple-rthm-seqs bars pitchcurves time-sig))
     (offset-lists (loop repeat 10
		     for v from 0
		     collect (mapcar #'(lambda (x)
					 (+ x v)) offset)))
     (set-offsets (loop for i in offset-lists
		     collect (case palette-type
			       (0 (koch-to-ps startpitch 10 :offset i))
			       (1 (koch-to-ps startpitch 10 :type2 t :offset i))
			       (t (dragon-to-ps startpitch 10 :offset i)))))
     (dr-palette (loop for p in set-offsets
		    for i from 1
		    collect (list i (list p))))
     (maplist (loop repeat required-bars
		   for i from 1
		   collect (1+ (mod i (length dr-palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "CA Rhythm Template"
		:composer "John Palma"
		:tempo-map `((1 (,base-unit ,tempo)))
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))))
		:set-palette  dr-palette
		:set-map `((1 ,(loop repeat (length maplist)
				  for l from 1
				  collect (1+ (mod l (length dr-palette))))))
		:rthm-seq-palette rseqs
		:rthm-seq-map `((1 ((pr ,(loop repeat (length maplist)
					    for i from 1
					    collect (1+ (mod i totalbars))))
				    (pl ,(loop repeat (length maplist)
				    	    for i from (floor totalbars 2)
				    	    collect (1+ (mod i totalbars)))))))
		:avoid-used-notes used-notes)))
  (midi-play template :midi-file (concatenate
				  'string storepath
				  (format nil "P2_~A_~D_~A_~D_scaled_~A.mid"
					  rev
					  tempo
					  (data time-sig)
					  rule
					  (list scale1 scale2))))))


(let* ((storepath "/tmp/")
       (messiaen '(f4 g4 a4 b4 cs5 ds5 f2 g2 a2 b2 cs3 ds3))
       (offset '(0 0 2))
       (offset-lists (loop repeat 10
			for v from 0
			collect (mapcar #'(lambda (x)
					    (+ x v)) offset)))
       (dragon-sets (loop for mp in messiaen
		       collect (loop for o in offset-lists
				 collect (dragon-to-ps mp 10 :offset o))))
       (dr-palettes (loop for p in dragon-sets
			 collect (make-set-palette 'dragon (loop for s in p
							      for i from 0
							      collect (list i (list s))))))
       (koch-sets (loop for mp in messiaen
		       collect (loop for o in offset-lists
				  collect (koch-to-ps mp 10 :offset o))))
       (koch2-sets (loop for mp in messiaen
		       collect (loop for o in offset-lists
				  collect (koch-to-ps mp 10 :type2 t :offset o))))
       (kh-palettes (loop for p in koch-sets
			 collect (make-set-palette 'koch (loop for s in p
							      for i from 0
							    collect (list i (list s))))))
       (kh2-palettes (loop for p in koch2-sets
			 collect (make-set-palette 'koch (loop for s in p
							      for i from 0
							    collect (list i (list s)))))))
  (print dragon-sets)
  (print (length dragon-sets))
  (print (length dr-palettes))
  (print (loop for i in dr-palettes
	    for j in messiaen
	    collect j))
  (loop for dr in dr-palettes
     for k1 in kh-palettes
     for k2 in kh2-palettes
     for i in messiaen
     do
       (gen-midi-chord-seq dr
			   (concatenate
			    'string storepath
			    (format nil "dragon-~A-palette.mid"
				    i)))
       (gen-midi-chord-seq k1
			   (concatenate
			    'string storepath
			    (format nil "koch-~A-palette.mid"
				    i)))
       (gen-midi-chord-seq k2
			   (concatenate
			    'string storepath
			    (format nil "koch2-~A-palette.mid"
				 i)))))


