;;;; parameter-optimization.lisp

(in-package #:parameter-optimization)

(defstruct gdist
  mu
  sigma
  weight)

(defparameter *gdists*
  (loop :repeat 2 :collect (make-gdist :mu 5d0 :sigma 2d0 :weight .5)))

(defun incw (n &optional (factor 1.1) (limit .7))
  (let* ((new-w (min (* (gdist-weight (nth n *gdists*))
			factor)
		     limit))
	 (other-w (- 1 new-w)))
    (if (zerop n)
	(setf (gdist-weight (nth 0 *gdists*)) new-w
	      (gdist-weight (nth 1 *gdists*)) other-w)
	(setf (gdist-weight (nth 0 *gdists*)) other-w
	      (gdist-weight (nth 1 *gdists*)) new-w))))

(defun choose-gdist ()
  (funcall (randist:make-discrete-random-var
	    (coerce (mapcar #'gdist-weight *gdists*) 'vector)
	    #(0 1))))

(defun draw (gdist)
  (randist:random-normal (gdist-mu gdist) (gdist-sigma gdist)))

(defun update (gdist score try)
  (setf (gdist-mu gdist) (- (gdist-mu gdist) (* (ecase score
						  (1 -.3)
						  (2 0.15)
						  (3 0)
						  (4 .6)
						  (5 .9))
						(- (gdist-mu gdist) try)))
	(gdist-sigma gdist) (let ((factor (ecase score
					    (1 1.15 ;1.35
					     )
					    (2 1.05 ;1.17
					     )
					    (3 1)
					    (4 .75)
					    (5 .3))))
			      (* (gdist-sigma gdist) factor))))

(defun print-stuff (active-n try)
  (progn (print *gdists*) (print active-n) (print try) (force-output)))

(defun main (&key (test nil))
  (loop :for i :upto 10
	:for active-n := (choose-gdist)
	:for try := (draw (nth active-n *gdists*))
	:for score := (progn (print-stuff active-n try)
			     (if test
				 (print (case (truncate try)
					  ((-2 4 6 12) 2)
					  ((-1 3 7 11) 3)
					  ((0 2 8 10) 4)
					  ((1 9) 5)
					  (t 1)))
				 (read))) 
	:for nearest := (if (<= (abs (- try (gdist-mu (nth 0 *gdists*))))
				(abs (- try (gdist-mu (nth 1 *gdists*)))))
			    0 1)
	:while (plusp score)
	:do (update (nth active-n *gdists*) score try)
	:do (incw nearest)))
