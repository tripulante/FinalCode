;; This creates the first piano instance 
;; selection mechanism: create permutations based in sorting

;; Dividir los ritmos por sección
;; s1 ritmos más lentos
;; (s s te q (e) h (q) s s 32 32 s te te)
;; (h (s) s e (e) w q (q) te s s 32 s te)
;; (e e e q (e) h (q) e (e) e te e e e)
;; (h (q) q q (e) e e (q) e e e (e) s e)
;; (s s e q (e) h (q) s s 32 32 s e e)

;; (1 0) (2 1) (0 2) (1 2)
;; (2 2) (1 1) (1 2) (4 3)
;; (3 4) (4 4)
(loop for i in '(piano piano-lh)
   do
     (set-slot 'chord-function
	       ;; 'play-all
               ;; 'viola-chord-selection-fun
	       'violin-chord-selection-fun
	       ;; 'chord-fun1
	       ;; 'chord-fun2
	       ;; 'piano-chord-fun
	       ;; 'string-chord-selection-fun
	       ;; 'second-chord
               i
               +slippery-chicken-standard-instrument-palette+))

(let*
    ((storepath "~/Documents/uni/Final/Code/MIDI/")
     (rev "Koch1-NoChord-A2")
     (tempo 112)
     (s1-total-bars (calculate-measures 10 tempo 5))
     (rhythms '(s s e q. (e) h. (q.) s s 32 32 s e e))
     (rhythms-lh '(h. (s) s e (e) w q. (q.) te s s 32 s te))
     (auto (make-instance 'automaton))
     (type 0) ; regular conway's
     
     (time-sig (make-time-sig '(6 8)))
     (generations (progn
		    (create-grid auto (length rhythms) (length rhythms))
		    (set-live-cells auto '((1 1) (2 1) (1 2) (4 3) (3 4) (4 4)
					   (4 0) (6 1) (4 2) (4 2)
					   (6 2) (5 1) (5 2) (8 3)
					   (7 4) (7 4)
					   ) :type type)
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
			      (loop repeat x
				   for i from 1
				 collect i)) attacklist))
     (rseqs (create-multiple-rthm-seqs bars pitchcurves time-sig))
     (totalbars (length bars))
     (basicset '(a b cs d e fs gs bf ef f g))
     (ca-pitchset (create-pc-live-cells livecells basicset))
     (offset '(0 0 2))
     (startpitch 'a2)
     (offset-lists (loop repeat 10
		     for v from 0
		     collect (mapcar #'(lambda (x)
					 (+ x v)) offset)))
     (koch2-offsets (loop for i in offset-lists
		       collect (koch-to-ps startpitch 10 :type2 nil :offset i)))
     (k-palette (loop for p in koch2-offsets
		   for i from 1
		   collect (list i (list p))))
     (palette (loop for i in ca-pitchset
		   for j from 1
		   collect (list j (list i))))
     (maplist (loop repeat s1-total-bars
		   for i from 1
		   collect (1+ (mod i (length k-palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "Superposiciones Piano I"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))
			     ))
		:tempo-map `((1 (q. ,tempo)))
		:set-palette k-palette
		:set-limits-low '((pr (0 c3 100 c3)))
		;; :set-limits-high '((pl (0 c3 100 c3)))
		:set-map `((1 ,maplist))
		:rthm-seq-palette rseqs
		:rthm-seq-map `((1 ((pr ,(loop repeat (length maplist)
					    for i from 1
					    collect (1+ (mod i totalbars))))
				    (pl ,(loop repeat (length maplist)
				    	    for i from (floor totalbars 2)
				    	    collect (1+ (mod i totalbars))))
				    ))
				)
		:avoid-used-notes nil
	     
		)))
  
  ;; (cmn-display template)
  (print ca-pitchset)
  (print lh-rhythms)
  (print generations)
  (midi-play template
	     :midi-file (concatenate
			 'string storepath
			 (format nil "Piano_~A_~D_~A_~D_~A.mid"
				 rev
				 tempo
				 (data time-sig)
				 type
				 sequential)))
  )
;; (s s e q (e) h (q) s s 32 32 s e e)
;; (h (s) s e (e) w q (q) e s s e s e)
;; (w h q q (e) e q e s s 32 32 s e e)

(let*
    ((storepath "~/Documents/uni/Final/Code/MIDI/")
     (rev "Koch1-NoChord-A2")
     (tempo 74)
     (s1-total-bars (calculate-measures 5 tempo 2))
     (rhythms '(e e e e e e e e s s s s s s s ))
     (rhythms-lh '(w h. q. q. (e) e q. e s s 32 32 s e e))
     (auto (make-instance 'automaton))
     (type 1) ; regular conway's
     (scale1 2)
     (scale2 2)
     (s1-rh (scale-rhythms scale1 rhythms))
     (s2-rh (scale-rhythms scale2 rhythms))
     (time-sig (make-time-sig '(6 8)))
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
			      (loop repeat x
				   for i from 1
				 collect i)) attacklist))
     (rseqs (create-multiple-rthm-seqs bars pitchcurves time-sig))
     (totalbars (length bars))
     (basicset '(a af b bf c cs d ds e ef f g))
     (ca-pitchset (create-pc-live-cells livecells basicset))
     (offset '(0 0 2))
     (startpitch 'a2)
     (offset-lists (loop repeat 10
		     for v from 0
		     collect (mapcar #'(lambda (x)
					 (+ x v)) offset)))
     (koch2-offsets (loop for i in offset-lists
		       collect (koch-to-ps startpitch 10 :type2 nil :offset i)))
     (k-palette (loop for p in koch2-offsets
		   for i from 1
		   collect (list i (list p))))
     (palette (loop for i in ca-pitchset
		   for j from 1
		   collect (list j (list i))))
     (maplist (loop repeat s1-total-bars
		   for i from 1
		   collect (1+ (mod i (length k-palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "Superposiciones Piano I"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))
			     ))
		:tempo-map `((1 (q. ,tempo)))
		:set-palette k-palette
		:set-limits-low '((pr (0 c3 100 c3)))
		;; :set-limits-high '((pl (0 c3 100 c3)))
		:set-map `((1 ,maplist))
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
  
  ;; (cmn-display template)
  (print ca-pitchset)
  (print lh-rhythms)
  (print generations)
  (midi-play template :midi-file (concatenate
			 'string storepath
			 (format nil "Piano_~A_~D_~A_~D_scaled_~A.mid"
				 rev
				 tempo
				 (data time-sig)
				 type
				 (list scale1 scale2))))
  )

