
(loop for i in '(piano piano-lh)
   do
     (set-slot 'chord-function
	       ;; 'play-all
               ;; 'viola-chord-selection-fun
	       ;; 'violin-chord-selection-fun
	       ;; 'chord-fun1
	       ;; 'chord-fun2
	       'piano-chord-fun
	       ;; 'string-chord-selection-fun
	       ;; 'second-chord
               i
               +slippery-chicken-standard-instrument-palette+))

;; #*01011010 -> 90
;; #*00011110 -> 30
;; #*00111100 -> 60
;; #*00111110 -> 62
;; #*01111110 -> 126
;; #*01100011 -> 99
(let*
    ((storepath "~/Documents/uni/Final/Code/MIDI/")
     (rev "Dragon-Chord-PianoCF")
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
     (rule 62)
     (time-sig (make-time-sig '(6 8)))
     (rule-vector (gethash rule rulehash))
     (current #*00000100000)
     (tempo 74)
     (s1-total-bars (calculate-measures 30 tempo 5))
     (base-rhythms '(s s e q. (e) q. e s 32))
     (scale1 1)
     (scale2 4)
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
			      (loop repeat x
				   for i from 1
				 collect (list i))) attacklist))
     (rseqs (create-multiple-rthm-seqs bars pitchcurves time-sig))
     (initial-set '(A4 AF2 AF3 AF4 B3 B4 BF4 C3 C4))
     (livecount (remove-duplicates (mapcar #'(lambda (x)
			      (reduce #'+ x)) generations)))
     (transposed-sets (mapcar #'data
			      (loop for i in livecount
				 collect (make-tl-set initial-set :transposition i))))

     (offset '(0 0 2))
     (startpitch 'cs4)
     
     (offset-lists (loop repeat 10
		     for v from 0
		     collect (mapcar #'(lambda (x)
					 (+ x v)) offset)))
     (dragon-offsets (loop for i in offset-lists
			collect (dragon-to-ps startpitch 10 :offset i)))
     (dr-palette (loop for p in dragon-offsets
					      for i from 1
					      collect (list i (list p))))
     
     (palette (loop for i in (mapcar #'(lambda (x)
					 (mapcar #'data x))
				     transposed-sets)
		 for counter from 1
		 collect (list counter
			       (list i))))
     (maplist (loop repeat s1-total-bars
		   for i from 1
		   collect (1+ (mod i (length dr-palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "CA Rhythm Template"
		:composer "John Palma"
		:tempo-map `((1 (q ,tempo)))
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))))
		:set-palette  dr-palette
		:set-map `((1 ,(loop repeat (length maplist)
				  for l from 1
				  collect (1+ (mod l (length palette))))))
		:rthm-seq-palette rseqs
		:rthm-seq-map `((1 ((pr ,(loop repeat (length maplist)
					    for i from 1
					    collect (1+ (mod i totalbars))))
				    (pl ,(loop repeat (length maplist)
				    	    for i from (floor totalbars 2)
				    	    collect (1+ (mod i totalbars))))
				    ))
				)
		;; :avoid-used-notes nil
	     
		)))
  ;; (print (length bars))
  ;; (cmn-display template)
  ;; (print rseqs)
  ;; (print palette)
  
  ;; (write-lp-data-for-all template :base-path "~/Documents/uni/Final/Code/Scores/")
  (midi-play template :midi-file (concatenate
				  'string storepath
				  (format nil "Piano_~A_~D_~A_~D_scaled_~A.mid"
					  rev
					  tempo
					  (data time-sig)
					  rule
					  (list scale1 scale2))) )
  ) 
