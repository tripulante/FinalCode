(defclass elementary-automata ()())

(nth 2 '(1 2 4))

create rules:
rule-array = convert rule number from 0 to 255 into a 8-bit binary number/array

map:
for an array a
get neighbours for i -> (list (1- i) i (1+ i))
if i = 0 -> ((nth (1- (length a)) a) i (1+ i))
elif i = (1- length a) -> ((1- i) i (first a))

calculate rule for neighbours

calculate rule:
j = convert neighbours to int
(setf (aref i a) (nth rule-array j))



(defmethod calculate-rule (values (rule-array vector))
  (let* ((val (loop for r in values
		   for ex from (1- (length values)) downto 0
		 sum (* r (expt 2 ex)) ))
	 (index (abs (- val (1- (length rule-array))))))
    (aref rule-array index)))

(defmethod getneighbours ((l vector) i)
  (let* ((last (1- (length l)))
	 ;; fix wraparound
	 (indexes (cond ((eq i 0)
			 (list last i (1+ i)))
			((eq i last)
			 (list (1- i) i 0))
			(t (list (1- i) i (1+ i))))))
    (loop for j in indexes
       collect (aref l j))))

(defmethod next-gen ((rule-vector vector) (current vector))
  (let* ((new-gen (make-array (length current)
			      :element-type 'bit
			      :initial-element '0)))
    (loop with len = (length current)
       for i from 1 below (1- len)
       do
	 ;; (print (list i (getneighbours current i)))
	 (setf
	   (bit new-gen i)
	   (calculate-rule
	    (getneighbours current i)
	    rule-vector)))
    new-gen))

(let* ((rule-vector #*00011110)
       (current #*0000001000000))
  (print (loop repeat 10
	      for i from 0
	    for v = current then (next-gen rule-vector v)
	    collect (list i v))))
  
