;; TODO
;; crear subgrupos en distintas métricas

(let*
    ((graph (make-instance 'graph))
     (numvertex 11)
     (edges '((0 1) (0 3) (4 5) (3 2) (1 9) (8 2) (7 9) (9 3)))
     (bfs (progn 
		 (create-adjacency-matrix graph numvertex)
		 (loop for e in edges
		    do
		      (add-am-edge graph (first e) (second e)))
		 (loop for i from 0 to 9
		      append (bfs graph i))))
     (dfs (loop for i from 0 to 9
	     append (explored graph)
	     do
	       (setf (explored graph) nil)
	       (dfs graph i)))
     (maplist (append bfs dfs))
     (template (make-slippery-chicken
		'+cycle+
		:title "Graph Template"
		:composer "John Palma"
		:ensemble '(((pr (piano :midi-channel 1))
			     (pl (piano-lh :midi-channel 2))))
		:set-palette '((0 ((f4 g4 a4 b4 cs5 ds5 f2 g2 a2 b2 cs3 ds3)))
			       (2 ((fs5 gs5 bf5 c5 d5 e5)))
			       (3 ((c4 df4 ef4 e4 gf4 g4 a4 bf4 c5)))
			       (4 ((c5 d5 ef5 e5 gf3 g2 af5 bf5 b5 c6)))
			       (5 ((c3 d3 df3 f3 gf3 g3 af3 b5 c6)))
			       (6 ((c4 df4 f4 gf4 g4 af4 b4 c5)))
			       (7 ((c4 d4 e4 f4 fs4 gs4 as4 b4 c5)))
			       (8 ((c2 df2 d2 ef2 f2 gf2 g2 af2 a2 b2 c3)))
			       (9 ((c4 df4 d4 ef4 e4 f4 fs4 g4 gs4 a4 as4 bf4 b4 c5)))
			       (10 ((c6 df6 d6 ef6 e6 f6 fs6 g6 gs6 a6 as6 bf6 b6 c6)))
			       (1 ((c6 df6 d6 ef6 e6 f6 fs6 g6 gs6 a6 as6 bf6 b6 c6)))
			       )
		:set-map `((1 ,maplist))
		:rthm-seq-palette `((0 ((((1 4) - 32 x 4 16 x 2 -))
					:pitch-seq-palette (,(procession 6 4))))
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
				    (1 ((((1 4) q))
					 :pitch-seq-palette ((5))))
				    (12 ((((2 4) h))
					 :pitch-seq-palette ((5))))
				    (13 ((((2 4) q e e))
					:pitch-seq-palette ((5 4 (1)))))
				    )
		:rthm-seq-map `((1 ((pr ,maplist)
				    (pl ,(reverse maplist)))))
		:avoid-used-notes nil
	     
		)))
  ;; (create-adjacency-matrix graph numvertex)
  ;; (cmn-display template)
  (print dfs)
  (print maplist)
  ;; (print generations)
  (write-lp-data-for-all template)
  (midi-play template)
  )
