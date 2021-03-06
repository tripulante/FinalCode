;; Functions in charge of generating piano material for Superposiciones
;; @file PianoFunctions.lsp
;; @author John Palma
;; Requires: Load the Math.lsp, Automata.lsp,
;; Elementals.lsp files



;; Creates a tailored instance of a sc piece for piano using CA and
;; the helper functions created before (fractal pitch sets)
;; Arguments
;; storepath: path to the folder where the file is to be stored
;; startpitch: initial pitch for the pitch set generation
;; tempo: tempo for the piece
;; tmin: time in minutes
;; rhythms: base rhythm set for the right hand piano, e.g.: '(e q q e (e) h)
;; rhythms-lh: base rhythm set for the left hand piano
;; time-sig: time signature for the set: '(5 4)
;; Key arguments
;; scale1: scale factor for right hand rhythms
;; scale2: scale factor for left hand rhythms
;; usechord: t if a chord function is to be used
;; palette-type: (0, 1, 2) for set palette generation. See Math.lsp
;; reverse-pc: t to create descending pitch curve for each bar.
;; offset: list of three integers for ofsetting the palette creation.
;; See Math.lsp
;; type: CA type. See Automata.lsp
;; used-notes: flag for :avoid-used-notes argument
;; in make-slippery-chicken
;; base-unit: Base beat unit for bar calculations
;; base-tempo-value: Number of beat units per bar. See Math.lsp
;; sequential: t to generate bars in sequence
;; use-lm-curves: use logistic maps to generate pitch curves. Overrides
;; reverse-pc
(defun piano1
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
       (type 0)
       (used-notes nil)
       (base-unit 'q.)
       (base-tempo-value 2)
       (sequential t)
       (use-lm-curves nil))
  (let*
    ((chord-fn (chord-function
		(get-data 'piano
			  +slippery-chicken-standard-instrument-palette+)))
     (chord-fn-l (chord-function
		(get-data 'piano-lh
			  +slippery-chicken-standard-instrument-palette+)))
     (rev (format nil "~A-~A-~A~A~A~A"
		  (case palette-type
		    (0 "K1")
		    (1 "K2")
		    (t "Drg"))
		  (if usechord
		      (list chord-fn chord-fn-l)
		      "")
		  startpitch
		  (if reverse-pc
		      "-R"
		      "")
		  (if use-lm-curves
		      "-LM"
		      "")
		  offset))
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
     (lh-rhythms (loop for r in lh-live ;rh-live
		    collect (nth r s1-rh)))
     (rh-rhythms (loop for r in rh-live
		    collect (nth r s2-rh)))
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
     (lm-curves (mapcar #'(lambda (x)
			    (loop for v in (logistic-curve 0.2 3.90 x)
			       for sv = (scale-value v 0. 1. 1 8)
			       collect (if usechord
					   (list sv)
					   sv))) attacklist))
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
     (rseqs (create-multiple-rthm-seqs bars
				       (if use-lm-curves
					   lm-curves
					   pitchcurves)
				       time-sig))
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
			 (format nil "P1_~A-~D-~A-~D-~A-~A.mid"
				 rev
				 tempo
				 (data time-sig)
				 type
				 (list scale1 scale2)
				 sequential)))))


;; Creates a tailored instance of a sc piece for piano using
;; Elementary CA and the helper functions created in Math.lsp
;; Arguments
;; storepath: path to the folder where the file is to be stored
;; startpitch: initial pitch for the pitch set generation
;; tempo: tempo for the piece
;; tmin: time in minutes
;; base-rhythms: base rhythm set for both piano hands
;; e.g.: '(e q q e (e) h)
;; rhythms-lh: base rhythm set for the left hand piano
;; time-sig: time signature for the set: '(5 4)
;; Key arguments
;; scale1: scale factor for right hand rhythms
;; scale2: scale factor for left hand rhythms
;; usechord: t if a chord function is to be used
;; palette-type: (0, 1, 2) for set palette generation. See Math.lsp
;; reverse-pc: t to create descending pitch curve for each bar.
;; offset: list of three integers for ofsetting the palette creation.
;; See Math.lsp
;; type: CA type. See Automata.lsp
;; used-notes: flag for :avoid-used-notes argument
;; in make-slippery-chicken
;; base-unit: Base beat unit for bar calculations
;; base-tempo-value: Number of beat units per bar. See Math.lsp
;; sequential: t to generate bars in sequence
;; use-lm-curves: use logistic maps to generate pitch curves. Overrides
;; reverse-pc
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
       (rule 90)
       (reverse-sets nil)
       (extra-info "")
       (use-lm-curves nil))
(let*
    ((chord-fn (chord-function
		(get-data 'piano
			  +slippery-chicken-standard-instrument-palette+)))
     (chord-fn-l (chord-function
		(get-data 'piano-lh
			  +slippery-chicken-standard-instrument-palette+)))
     (rev (format nil "~A-~A-~A~A~A~A~A"
		  (case palette-type
		    (0 "K1")
		    (1 "K2")
		    (t "Drg"))
		  (if usechord
		      (list chord-fn chord-fn-l)
		      "")
		  startpitch
		  (if reverse-pc
		      "-R"
		      "")
		  (if use-lm-curves
		      "-LM"
		      "")
		  offset
		  (if reverse-sets
		      "-RST"
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
     (current (make-array (+ 2 (length base-rhythms))
			  :element-type 'bit
			  :initial-contents (loop with r = (+ 2 (length base-rhythms))
					       repeat r
					       for i from 0
					       collect (if (= i (floor r 2))
							   1
							   0))))
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
     (lm-curves (mapcar #'(lambda (x)
			    (loop for v in (logistic-curve 0.2 3.891 x)
			       for sv = (scale-value v 0. 1. 1 8)
			       collect (if usechord
					   (list sv)
					   sv))) attacklist))
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
     (rseqs (create-multiple-rthm-seqs bars
				       (if use-lm-curves
					   lm-curves
					   pitchcurves)
				       time-sig))
     (offset-lists (loop repeat 10
		     for v from 0
		     collect (mapcar #'(lambda (x)
					 (+ x v)) offset)))
     (set-offsets (loop for i in offset-lists
		     collect (case palette-type
			       (0 (koch-to-ps startpitch 10 :offset i))
			       (1 (koch-to-ps startpitch 10 :type2 t :offset i))
			       (t (dragon-to-ps startpitch 10 :offset i)))))
     (dr-palette (loop with sets = (if reverse-sets
				    (reverse set-offsets)
				    set-offsets)
		    for p in sets
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
				  (format nil "P2_~A_~D_~A_~D_scaled_~A~A.mid"
					  rev
					  tempo
					  (data time-sig)
					  rule
					  (list scale1 scale2)
					  extra-info)))))





