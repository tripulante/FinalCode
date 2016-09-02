;; Example tests for the Piano functions
;; @file PianoTests.lsp
;; @author John Palma
;; Requires: Load the Math.lsp, Automata.lsp,
;; Elementals.lsp and PianoFunctions.lsp files


;; This example creates a file starting in f2. Tempo 112,
;; 5 minutes long in (5 4). Uses chord functions and
;; reverse pitch curves
(piano1 "/tmp/"
	'f2
	112
	5
	'(s s e q. (e) h. (q.) s s 32 32 s e e)
	'(h. (s) s e (e) w q. (q.) te s s 32 s te)
	(make-time-sig '(5 4))
	:base-unit 'q
	:base-tempo-value 5
	:reverse-pc t
	:usechord t
	:used-notes t)

;; This example creates a file starting in c5 with offsets using
;; Elementary CA.
;; Tempo 112, 5 minutes long in (6 8).
;; Reverses the pitch sets in order and ascending curves.
;; Uses a Koch curve for set generation and scales the original
;; rhythmic units
(piano2 "/tmp/"
	'c5
	112
	5
	'(e (e) e (e) e (e) e (e) s (s) s (s) s (s) s)
	(make-time-sig '(6 8))
	:base-unit 'q
	:base-tempo-value 5
	:scale1 0.5
	:scale2 1
	:usechord nil
	:reverse-pc nil
	:use-lm-curves nil
	:palette-type 0
	:offset '(-2 3 -4)
	:reverse-sets t)
