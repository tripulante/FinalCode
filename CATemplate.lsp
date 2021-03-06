;; TODO
;; crear subgrupos en distintas métricas

(let*
    ((rule-vector #*10110110)
     (current #*0000000000100)
     (generations (loop repeat 100
		     for i from 0
		     for v = current then (next-gen rule-vector v)
		     collect v))
     (maplist (loop for g in generations
		 append (loop for i from 1 below (1- (length g))
			   for v = (aref g i)
			   if (/= v 0)
			   collect i)))
     (template (make-slippery-chicken
		'+cycle+
		:title "Template"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))))
		:set-palette '((1 ((f4 g4 a4 b4 cs5 ds5 f2 g2 a2 b2 cs3 ds3)))
			       (2 ((fs5 gs5 bf5 c5 d5 e5)))
			       (3 ((c4 df4 ef4 e4 gf4 g4 a4 bf4 c5)))
			       (4 ((c5 d5 ef5 e5 gf3 g2 af5 bf5 b5 c6)))
			       (5 ((c3 d3 df3 f3 gf3 g3 af3 b5 c6)))
			       (6 ((c4 df4 f4 gf4 g4 af4 b4 c5)))
			       (7 ((c4 d4 e4 f4 fs4 gs4 as4 b4 c5)))
			       (8 ((c2 df2 d2 ef2 f2 gf2 g2 af2 a2 b2 c3)))
			       (9 ((c4 df4 d4 ef4 e4 f4 fs4 g4 gs4 a4 as4 bf4 b4 c5)))
			       (10 ((c6 df6 d6 ef6 e6 f6 fs6 g6 gs6 a6 as6 bf6 b6 c6)))
			       (11 ((c6 df6 d6 ef6 e6 f6 fs6 g6 gs6 a6 as6 bf6 b6 c6)))
			       )
		:set-map `((1 ,maplist))
		:rthm-seq-palette `((1 ((((1 4) 32 x 8))
					:pitch-seq-palette (,(procession 8 4))))
				    (2 ((((1 4) { 3 (te) { 3 (18) 36 } { 3 - 36 36 36 - } }))
					:pitch-seq-palette (,(procession 4 4))))
				    (3 ((((1 4) (1 ((4 (1 (1) 1 1 1)) (5 (1 1 1 1))))))
					:pitch-seq-palette (,(procession 8 10))))
				    (4 ((((1 4) (1 (1 (3 (1 1 1 1))))))
					:pitch-seq-palette (,(procession 5 10))))
				    (5 ((((1 4) (1 (2 (3 (1 1 1))))))
					:pitch-seq-palette (,(procession 4 10))))
				    (6 ((((1 4) (1 (1 (3 (1 1 1))))))
					:pitch-seq-palette (,(procession 4 10))))
				    (7 ((((1 4) (1 ((4 (1 (1) 1 1 1)) (3 (1 1 (1) 1))))))
					:pitch-seq-palette (,(reverse (procession 7 20)))))
				    (8 ((((1 4) (1 ((2 (1 (2) 1 1)) (3 (1 1 (1) 1))))))
					:pitch-seq-palette (,(procession 6 '((1) 3 5 10 (2) 1)))))
				    (9 ((((1 4) (1 ((1) (3 (1 1 1 1))))))
					:pitch-seq-palette (,(procession 4 '((10) 2 (2) 1)))))
				    (10 ((((1 4) (1 ((1) (1 (1 1))))))
					 :pitch-seq-palette (((10) (2)))))
				    (11 ((((1 4) q))
					:pitch-seq-palette ((5))))
				    )
		:rthm-seq-map `((1 ((pr ,maplist)
				    (pl ,(reverse maplist)))))
		:avoid-used-notes nil
	     
		)))
  
  ;; (cmn-display template)
  (print maplist)
  (print generations)
  (write-lp-data-for-all template)
  (midi-play template)
  )
