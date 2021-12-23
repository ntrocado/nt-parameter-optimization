;;;; parameter-optimization.lisp

(in-package #:parameter-optimization)

(defstruct variate
  mu
  sigma
  weight)

(defun create-pdf (n-variates mu sigma)
  (loop :repeat n-variates
	:collect (make-variate :mu mu :sigma sigma :weight (/ 1 n-variates))))

(defun mu-score-factor (score)
  (ecase score
    (1 -.3)
    (2 0.15)
    (3 0)
    (4 .6)
    (5 .9)))

(defun sigma-score-factor (score)
  (ecase score
    (1 1.15)
    (2 1.05)
    (3 1)
    (4 .75)
    (5 .3)))

(defun new-mu (var score last-try)
  (let ((mu (variate-mu var)))
    (- mu
       (* (mu-score-factor score)
	  (- mu last-try)))))

;;;TODO: when score is high but new mu is far from previous mu, sigma shouldn't decrease
(defun new-sigma (var score)
  (* (variate-sigma var)
     (sigma-score-factor score)))

(defun new-weight (pdf var last-try)
  (if (eq var (first (sort (copy-seq pdf) #'< :key (lambda (x)
						     (abs (- last-try
							     (variate-mu x)))))))
      (min .7 (* (variate-weight var) 1.1))
      (max .1 (* (variate-weight var) .9))))

(defun choose-variate (pdf)
  (funcall (randist:make-discrete-random-var
	    (coerce (mapcar #'variate-weight pdf) 'vector)
	    (coerce pdf 'vector))))

(defun new-pdf (pdf score last-try active)
  (substitute (make-variate :mu (new-mu active score last-try)
			    :sigma (new-sigma active score)
			    :weight (new-weight pdf active last-try))
	      active
	      pdf))

(defun draw (var)
  (randist:random-normal (coerce (variate-mu var) 'double-float)
			 (coerce (variate-sigma var) 'double-float)))

(defmacro opt ((parameters &key (verbose nil) (limit nil)) &body body)
  "Optimize PARAMETERS, a list of the form:

 ((parameter-1 number-of-vars-1 mean-1 std-dev-1)
  (parameter-2 number-of-vars-2 mean-2 std-dev-2)
  ...
  (parameter-n number-of-vars-n mean-n std-dev-n))

For each parameter, all variates are initialized with the same given mean and standard deviation.

Repeatedly execute BODY, each time binding the parameters to new values and reading a score from the user. The score must be a number between 1 (bad match) and 5 (perfect match), or 0, which stops immediately. If LIMIT is set, execution only continues for that maximum number of tries."
  (alexandria:with-gensyms (score n)
    (let ((for-clauses (mapcan (lambda (parm)
				 (alexandria:with-gensyms (pdf-name active-name)
				   `(:for ,pdf-name := (create-pdf ,(second parm) ,(third parm) ,(fourth parm))
				     :then (new-pdf ,pdf-name ,score ,(first parm) ,active-name)
				     :for ,active-name := (choose-variate ,pdf-name)
				     :for ,(first parm) := (draw ,active-name)
				     :for ,(gensym) := ,(when verbose
							  `(format t "~a~%pdf: ~a~%"
								   ,(symbol-name (first parm))
								   ,pdf-name)))))
			       parameters)))
      `(loop :for ,n :from 1
	     ,@for-clauses
	     :for ,score := (progn ,@body (read))
	     :until (or (zerop ,score) (and ,limit (= ,n ,limit)))))))
