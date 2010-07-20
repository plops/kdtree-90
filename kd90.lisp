(defstruct node
  (bucket)
  (cutdim)
  (cutval)
  (loson)
  (hison)
  (lopt)
  (hipt))

(defconstant +dim+ 2)
(defparameter *points* nil)
(defparameter *perm* nil)

(defun px (i j)
  (aref (aref *points* (aref *perm* i)) j))

(defun find-max-spread (l u)
  (let ((spread 0)
	(dim 0))
    (loop for j from 0 below +dim+ do
	 (let* ((mi (loop for i from l upto u minimize (px i j)))
		(ma (loop for i from l upto u maximize (px i j)))
		(v (- ma mi)))
	   (when (< spread v)
	     (setf spread v
		   dim j))))
    dim))

(defun swap (a b)
  (let ((h (aref *perm* a)))
    (setf (aref *perm* a) (aref *perm* b)
	  (aref *perm* b) h)))

(defun select (l u m cutdim)
  (labels ((p (l) ;; accessor
	     (px l cutdim)))
    (format t "los gehts~%")
    (loop
       (format t "~a~%" (list 'loop l u))
       (if (<= u (1+ l))
	   (progn
	     (when (and (= u (1+ l))
			(< (p u) (p l)))
	       (swap u l))
	     (return-from select (aref *perm* m)))
	   (let ((mid (floor (+ u l) 2)))
	     (swap mid (1+ l))
	     ;; rearrange for p[l]<=p[l+1]<=p[u]
	     (labels ((enforce-order (l u)
			(when (< (p u) (p l)) (swap u l))))
	       (enforce-order l u)
	       (enforce-order (1+ l) u)
	       (enforce-order l (1+ l)))
	     (let* ((i (1+ l))
		    (j u)
		    (ia (aref *perm* i))
		    (a (p i)))
	       (loop ;; scan up and down, put big elements right, small
		  ;; ones left
		  (format t "~a~%" (list 'loopij i j))
		  (loop do (incf i) while (< (p i) a))
		  (loop do (decf j) while (< a (p j)))
		  (when (< j i) (return))
		  (swap i j))
	       (setf (aref *perm* (1+ l)) (aref *perm* j)
		     (aref *perm* j) ia)
	       (when (<= m j) (setf u (1- j)))
	       (when (<= j m) (setf l i))))))))
#+nil
(let* ((n 10)
       (*perm* (make-array n :element-type 'fixnum
			   :initial-contents (loop for i below n collect i)))
       (*points* (make-array n :element-type '(array * (#.+dim+))
			     :initial-contents
			     (loop for i below n collect
				  (make-array +dim+
					      :initial-contents
					      (loop repeat +dim+ collect
						   (random 1d0)))))))
  (select 0 (1- n) (floor (+ 0 (1- n)) 2)
	  0)
  (loop for i below n collect (list (read-from-string
				     (format nil "~2,2f" (px i 0))))))


(defun build (l u)
  (if (<= (1+ (- u l)) 2)
      (make-node :bucket t
		 :lopt l
		 :hipt u)
      (let ((cutdim (find-max-spread l u))
	    (m (floor (+ l u) 2)))
	(select l u m cutdim)
	(make-node :bucket nil
		   :cutdim cutdim 
		   :cutval (px m cutdim)
		   :loson (build l m)
		   :hison (build (1+ m) u)))))

#+nil
(time
 (let* ((n 300))
   (setf 
    *perm* (make-array n :element-type 'fixnum
		       :initial-contents (loop for i below n collect i))
    *points* (make-array n :element-type '(array * (#.+dim+))
			 :initial-contents
			 (loop for i below n collect
			      (make-array +dim+
					  :initial-contents
					  (loop repeat +dim+ collect
					       (random 1d0))))))
   (defparameter *tree* (build 0 (1- n)))))

(defun distance2 (i j)
  (let ((sum 0))
    (dotimes (k +dim+)
      (let ((v (- (px i k) (px j k))))
	(incf sum (* v v))))
    sum))

(defun nn (target tree perm)
  (let ((dist 1e20)
	(nearest 0))
    (labels ((rec (node)
	       (with-slots (bucket lopt hipt cutval cutdim loson hison)
		   node
		(if bucket
		    (loop for i from lopt upto hipt do
			 (let ((d (distance2 (aref perm i) target)))
			   (when (< d dist)
			     (setf dist d
				   nearest (aref perm i)))))
		    (let ((val cutval)
			  (x (px target cutdim)))
		      (if (< x val)
			  (progn (rec loson)
				 (when (< val (+ x dist))
				   (rec hison)))
			  (progn (rec hison)
				 (when (< (- x dist) val)
				   (rec loson)))))))))
      (rec tree))
    nearest))

#+nil
(nn 5 *tree* *perm*)

(defun draw-tree (root minx maxx miny maxy)
  (let ((boxes (list (list minx maxx miny maxy))))
   (labels ((rec (root)
	      (with-slots (bucket hison loson
				  cutdim cutval)
		  root
		(unless bucket
		  (ecase cutdim
		    (0 (setf maxy cutval))
		    (1 (setf minx cutval)))
		  (push (list minx maxx miny maxy) boxes)
		  (rec hison)
		  (ecase cutdim
		    (0 (setf miny cutval))
		    (1 (setf maxx cutval)))
		  (push (list minx maxx miny maxy) boxes)
		  (rec loson)))))
     (rec root))
   (loop for b in boxes collect
	(destructuring-bind (a b c d)
	    b
	  (list (read-from-string (format nil "~2,4f" a))
		(read-from-string (format nil "~2,4f" b))
		(read-from-string (format nil "~2,4f" c))
		(read-from-string (format nil "~2,4f" d)))))))

#+nil
(defparameter *boxes*
 (draw-tree *tree* 0 1 0 1))


(defun eps-moveto (x y)
  (format nil "~f ~f moveto~%" x y))

(defun eps-lineto (x y)
  (format nil "~f ~f lineto~%" x y))

(defun eps-rectangle (box)
  (destructuring-bind (x0 y0 x y)
      box
   (format nil "newpath~%~a~a~a~a~astroke~%"
	   (eps-moveto x0 y0)
	   (eps-lineto x y0)
	   (eps-lineto x y)
	   (eps-lineto x0 y)
	   (eps-lineto x0 y0))))


(defun eps-tree (boxes)
  (with-open-file (s "/home/martin/tmp/tree.eps" :direction :output
		     :if-exists :supersede)
    (format s "%!PS-Adobe-3.0
%%Pages: 1
%%BoundingBox: 0 0 700 500
%%EndComments
400.0 400.0 scale
0.002 setlinewidth
0 setgray~%")
   (loop for b in boxes do
	(format s "~a" (eps-rectangle b)))
   (format s "%%EOF")))

#+nil
(eps-tree)