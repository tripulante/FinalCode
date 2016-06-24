;; This creates the first piano instance 
;; selection mechanism: create permutations based in sorting

;; Dividir los ritmos por sección
;; s1 ritmos más lentos

(let*
    ((tempo 180)
     (s1-total-bars (calculate-measures 3 tempo 2))
     (s2-total-bars (calculate-measures 4 tempo 2))
     (rhythms '(s s e q (e) h (q) s s 32 32 s e e))
     (rhythms-lh '(h (s) s e (e) w q (q) e s s 32 s e))
     (s1-rh (scale-rhythms 1 rhythms-lh))
     (s2-rh (scale-rhythms 2 s1-rh))
     (auto (make-instance 'automaton))
     (type 0) ; regular conway's
     
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
     (lh-bar-set (multiple-value-list
		  (create-bars ;;augmentation-test
			       (values-to-rhythms lh-rhythms)
			       time-sig
			       :sequential t)))
     (rh-bar-set (multiple-value-list
		  (create-bars (values-to-rhythms rh-rhythms)
			       time-sig
			       :sequential t)))
     (lh-pitchcurves (mapcar #'(lambda (x)
				 (loop repeat x
				    for i from 1
				    collect i))
			     (mapcar #'get-attacks (second lh-bar-set))))
     (rh-pitchcurves (mapcar #'(lambda (x)
				 (loop repeat x
				    for i downfrom 10
				    collect i))
			     (mapcar #'get-attacks (second rh-bar-set))))
     (totalbars (+ (length (third lh-bar-set))
		   (length (third rh-bar-set))))
     (rseqs (append (create-multiple-rthm-seqs
		     (third lh-bar-set) lh-pitchcurves time-sig)
		    (create-multiple-rthm-seqs
		     (third rh-bar-set) rh-pitchcurves time-sig
		     :start-id (1+ (length (third lh-bar-set))))))
     (basicset '(a af b bf c cs d ds e ef f g))
     (ca-pitchset (create-pc-live-cells livecells basicset))
     (palette (loop for i in ca-pitchset
		   for j from 1
		   collect (list j (list i))))
     (maplist (loop repeat s1-total-bars
		   for i from 1
		   collect (1+ (mod i (length palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "Superposiciones Piano I"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))
			     ))
		:tempo-map `((1 (q. ,tempo)))
		:set-palette palette
		:set-limits-low '((pr (0 c3 100 c3)))
		;; :set-limits-high '((pl (0 c3 100 c3)))
		:set-map `((1 ,maplist))
		:rthm-seq-palette rseqs
		:rthm-seq-map `((1 ((pr ,(loop repeat (length maplist)
					    for i from 1
					    collect (1+ (mod i totalbars))))
				    (pl ,(loop repeat (length maplist)
				    	    for i from (length (third lh-bar-set))
				    	    collect (1+ (mod i totalbars))))
				    ))
				)
		;; :avoid-used-notes nil
	     
		)))
  
  ;; (cmn-display template)
  (print ca-pitchset)
  (print lh-rhythms)
  (print generations)
  (midi-play template )
  )


(let*
    ((tempo 180)
     (s1-total-bars (calculate-measures 4 tempo 2))
     (s2-total-bars (calculate-measures 4 tempo 2))
     (rhythms '(s s e q (e) h (q) s s 32 32 s e e))
     (rhythms-lh '(h (s) s e (e) w q (q) e s s 32 s e))
     (auto (make-instance 'automaton))
     (type 1) ; regular conway's
     
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
		    collect (nth r rhythms)))
     (rh-rhythms (loop for r in rh-live
		    collect (nth r rhythms-lh)))
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
     (palette (loop for i in ca-pitchset
		   for j from 1
		   collect (list j (list i))))
     (maplist (loop repeat s1-total-bars
		   for i from 1
		   collect (1+ (mod i (length palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "Superposiciones Piano I"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))
			     ))
		:tempo-map `((1 (q. ,tempo)))
		:set-palette palette
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
  (midi-play template )
  )

(let*
    ((tempo 180)
     (s1-total-bars (calculate-measures 4 tempo 2))
     (rhythms '(s s e q (e) h (q) s s 32 32 s e e))
     (rhythms-lh '(h (s) s e (e) w q (q) e s s e s e))
     (auto (make-instance 'automaton))
     (type 0) ; regular conway's
     (s1-rh (scale-rhythms 2 rhythms))
     (s2-rh (scale-rhythms 0.75 rhythms-lh))
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
     (test (print ca-pitchset))
     (palette (loop for i in ca-pitchset
		   for j from 1
		   collect (list j (list i))))
     (maplist (loop repeat s1-total-bars
		   for i from 1
		   collect (1+ (mod i (length palette)))))
     (template (make-slippery-chicken
		'+cycle+
		:title "Superposiciones Piano I"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))
			     ))
		:tempo-map `((1 (q. ,tempo)))
		:set-palette palette
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
  (midi-play template )
  )

