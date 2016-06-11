(defclass math-operations ()
  ((subresults :accessor m91sub :initform nil)))

;; (defmethod bubble-pulsation-adiabatic (gamma p0 rho r0)
;;   (* (/ 1 (* 2 pi r0)) (sqrt (/ (* 3 gamma p0) rho))))
;; (defmethod bubble-pulsation-isothermal (sigma p0 rho r0)
;;   (* (/ 1 (* 2 pi r0)) (sqrt
;; 			(+ (/ (* 3 p0) rho) (/ (* 4 sigma) (* rho r0))))))
(defmethod bubble-pulsation ((m math-operations) a p0 rho r0)
  (let ((adiabatic (* (/ 1 (* 2 pi r0))
		      (sqrt (/ (* 3 a p0) rho))))
	(isothermal (* (/ 1 (* 2 pi r0))
		       (sqrt
			(+ (/ (* 3 p0) rho)
			   (/ (* 4 a) (* rho r0)))))))
    (values adiabatic isothermal)))

;; chaotic functions

;; this function implements a logistic map
(defmethod logistic-map (x r)
  (let* ((r1 (cond ((< r 0) 0)
		   ((>= r 4) (- 4 1e-6))
		   (t r))))
    (print (list r1 r))
    (* x r1 (- 1 x))))
;; Linear Interpolation
(defmethod scale-value (value oldMin oldMax newMin newMax)
  (+ (floor (* (- value oldMin)
	   (- newMax newMin)) (- oldMax oldMin)) newMin))
;; Runs the logistic map n times, starting from an initial x0
;; value and then scales it to (min max) values
;; no error checking
(defmethod logistic-curve (x0 r n min max)
  (let* ((xi (cond ((< x0 0) 0)
		   ((> x0 1) 1)
		   (t x0))))
    (print (list xi x0))
    (loop repeat n
       for x = (logistic-map xi r)
       then (logistic-map x r)
       collect x)))

;; lorenz system
(defmethod lorenz-system (n sigma beta ro init)
  (loop repeat n
     with (x0 y0 z0) float = '(1.0 1.0 1.0)
     for x = (+ x0 (* init (* sigma (- y0 x0))))
     for y = (+ y0 (* init (- (* x0 (- ro z0)) y0)))
     for z = (+ z0 (* init (- (* x0 y0) (* z0 beta))))
     collect (list x y z)
     do
       (setf x0 x
	     y0 y
	     z0 z)))

(defmethod rossler-attractor (n a b c inc &key (x1 1.0) (y1 1.0) (z1 1.0))
  (loop repeat n
     with (x0 y0 z0) float = (list x1 y1 z1)
     for x = (+ x0 (* inc (- (- y0) z0)))
     for y = (+ y0 (* inc (+ x0 (* a y0))))
     for z = (+ z0 (* inc (+ b (* z0 (- x0 c)))))
     collect (list x y z)
     do
       (setf x0 x
	     y0 y
	     z0 z)))

(defun double-pendulum (n m1 m2 l1 l2 t1 t2 &key (time 0.01) (g 9.81))
  (loop repeat n
     with theta1 = t1
     with theta2 = t2
     for delta = (- theta1 theta2)
     with d1theta1 = 0.0
     with d1theta2 = 0.0
     for d2theta1 = (/ (+ (* m2 l1 (expt d1theta1 2)
			     (sin delta) (cos delta))
			  (* m2 g (sin theta2) (cos delta))
			  (* m2 l2 (expt d1theta2 2) (sin delta))
			  (- (* (+ m1 m2) g (sin theta1))))
		       (- (* l1 (+ m1 m2))
			  (* m2 l2
			     (expt (cos delta) 2))))
     for d2theta2 = (/ (+ (- (* m2 l2 (expt d1theta2 2)
				(sin delta) (cos delta)))
			  (* (+ m1 m2)
			     (- (* g (sin theta1) (cos delta))
				(* l1 (expt d1theta1 2) (sin delta))
				(* g (sin theta2)))))
		       (- (* (+ m1 m2) l2)
			  (* m2 l2
			     (expt (cos delta) 2))))
     for x1 = (* l1 (sin theta1))	; in radians
     for y1 = (- (* l1 (cos theta1)))
     for x2 = (+ (* l1 (sin theta1)) (* l2 (sin theta2)))
     for y2 = (- (- (* l1 (cos theta1))) (* l2 (cos theta2))) 
     collect (list x1 y1 x2 y2)
     do
       (setf d1theta1 (+ d1theta1 (* time d2theta1))
	     d1theta2 (+ d1theta2 (* time d2theta2))
	     theta1 (+ theta1 (* time d1theta1))
	     theta2 (+ theta2 (* time d1theta2)))))

(defun calculateTotalTime (measures beatunit tempo)
  (/ (* measures beatunit) tempo))

(defun calculateMeasures (total tempo beatunit)
  (/ (* total tempo) beatunit))

(defun calculateBeatUnit (total measures tempo)
  (/ (* total tempo) measures))

(defun calculateTempo (total measures beatunit)
  (/ (* measures beatunit) total))

;; recursive functions
(defmethod m91 ((m math-operations) n)
  (if (> n 100)
      (- n 10)
      (progn
	(setf (m91sub m) (append (m91sub m) (list n)))
	(m91 m (m91 m (+ n 11))))))

(let* ((m (make-instance 'math-operations)))
  ;; (print (multiple-value-list (bubble-pulsation m 30000 10000 1 8)))
  (print (m91 m -500))
  (print (m91sub m))
  (print (logistic-curve 0.1 3.96 5 0 6)))

(let* ((sigma 10)
       (beta (/ 8 3))
       (ro 28)
       (init 0.01)
       (a 0.2)
       (b 0.2)
       (c 5.7))
(defun lorenz-x (x y sigma init)
  (+ x
     (* init (* sigma (- y x)))))

(defun lorenz-y (x y z ro init)
  (+ y
     (* init (- (* x (- ro z)) y))))

(defun lorenz-z (x y z beta init)
  (+ z
     (* init (- (* x y) (* z beta)))))

(defun lorenz-system (n sigma beta ro init)
  (loop repeat n
     with (x0 y0 z0) float = '(1.0 1.0 1.0)
     for x = (+ x0 (* init (* sigma (- y0 x0))))
     for y = (+ y0 (* init (- (* x0 (- ro z0)) y0)))
     for z = (+ z0 (* init (- (* x0 y0) (* z0 beta))))
     collect (list x y z)
     do
       (setf x0 x
	     y0 y
	     z0 z)))

(defmethod rossler-attractor (n a b c inc &key (x1 1.0) (y1 1.0) (z1 1.0))
  (loop repeat n
     with (x0 y0 z0) float = (list x1 y1 z1)
     for x = (+ x0 (* inc (- (- y0) z0)))
     for y = (+ y0 (* inc (+ x0 (* a y0))))
     for z = (+ z0 (* inc (+ b (* z0 (- x0 c)))))
     collect (list x y z)
     do
       (print (list x0 y0 z0 x1 y1 z1))
       (setf x0 x
	     y0 y
	     z0 z)))

(print (lorenz-system 5 sigma beta ro init))
(print (rossler-attractor 5 a b c init :z1 0.0))
  )


(let* ((m1 5)
       (m2 3)
       (l1 5)
       (l2 7)
       (t1 (/ (* 90 pi) 180))
       (t2 (/ (* 90 pi) 180)))
(print (double-pendulum 100 m1 m2 l1 l2 t1 t2))
)


Double Pendulum:

with m1, m2, l1, l2, g, theta1




x1 = (* l1 (sin theta1)) ; in radians
y1 = (- (* l1 (cos theta1)))
x2 = (* l1 (sin theta1)) + (* l2 (sin theta2))
y2 = (- (- (* l1 (cos theta1))) (* l2 (cos theta2)))

delta = (- theta1 theta2)
mu = (1+ (/ m1 m2))

d2theta1 = (/ (+ (* m2 l1 (expt d1theta1 2) (sin delta) (cos delta))
		 (* m2 g (sin theta2) (cos delta))
		 (* m2 l2 (expt d1theta2 2) (sin delta))
		 (- (* (+ m1 m2) g (sin theta1)))) (- (* l1 (+ m1 m2))
						      (* m2 l2
							 (expt (cos delta) 2))))

d2theta2 = (/ (+ (- (* m2 l2 (expt d1theta2 2) (sin delta) (cos delta)))
		 (* (+ m1 m2)
		    (- (* g (sin theta1) (cos delta))
		       (* l1 (expt d1theta1 2) (sin delta))
		       (* g (sin theta2))))) (-
					      (* (+ m1 m2) l2)
					      (* m2 l2
						 (expt (cos delta) 2))))

;; d2Theta1 = (g*(Math.sin(Theta2)*Math.cos(Theta1-Theta2)-mu*Math.sin(Theta1))-(l2*dTheta2*dTheta2+l1*dTheta1*dTheta1*Math.cos(Theta1-Theta2))*Math.sin(Theta1-Theta2))/(l1*(mu-Math.cos(Theta1-Theta2)*Math.cos(Theta1-Theta2)));
;;   d2Theta2  =  (mu*g*(Math.sin(Theta1)*Math.cos(Theta1-Theta2)-Math.sin(Theta2))+(mu*l1*dTheta1*dTheta1+l2*dTheta2*dTheta2*Math.cos(Theta1-Theta2))*Math.sin(Theta1-Theta2))/(l2*(mu-Math.cos(Theta1-Theta2)*Math.cos(Theta1-Theta2)));


d1theta1 = (+ d1theta1 (* time d2theta1))
d1theta2 = (+ d1theta2 (* time d2theta2))
theta1 = (+ theta1 (* time d1theta1))
theta2 = (+ theta2 (* time d1theta2))

