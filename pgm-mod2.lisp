;;; -------------------------------------------------------------- ;;;
;;; UTILITIES
;;; -------------------------------------------------------------- ;;;

(defun seq (start end &optional (step 1))
  "Creates a ranges"
  (let ((results nil)
	(partial start))
    (cond ((and (< start end)
		(plusp step))
	   (loop while (< partial end) do
	     (push partial results)
	     (incf partial step)))
	  ((and (> start end)1
		(minusp step))
	   (loop while (> partial end) do
	     (push partial results)
	     (incf partial step)))
	  (t
	   nil))
    (reverse results)))
	  

(defun pick (lst)
  "Picks up an element from a list"
  (when  (listp lst)
    (elt lst (random (length lst)))))


(defun scramble (lst &optional (sofar nil))
  "Scrambles a list of different elements"
  (if (null lst)
      sofar
    (let ((picked (pick lst)))
      (scramble (remove picked lst) (cons picked sofar)))))


(defun scramble* (lst)
  "Scrambles any list of objects"
  (let ((l (length lst))
        (pos nil))
    (dotimes (i l)
      (push i pos))
    (mapcar #'(lambda (x) (elt lst x)) (scramble pos))))


;;; -------------------------------------------------------------- ;;;
;;; GENERATING PRODUCTIONS
;;; -------------------------------------------------------------- ;;;

(defparameter *buffers* '(visual imaginal goal retrieval manual))

(defparameter *slots* '(slot1 slot2 slot3))

(defparameter *template-modify*
	      "
(p ~A 
 ?manual> 
  preparation free 
  processor free 
  execution free 
 ?retrieval> 
  state free 
 ?imaginal> 
  state free 
 ?goal> 
  state free 
 ?visual>
  state free 
 =~A>
 =~A> 
  ~A =X 
==> 
 =~A> 
  ~A =X)"
	      )

(defparameter *template-create-manual*
	      "
(p ~A 
 ?manual> 
  preparation free 
  processor free 
  execution free 
 ?retrieval> 
  state free 
 ?imaginal> 
  state free 
  buffer full
 ?goal> 
  state free 
  buffer full
 ?visual>
  state free
 =~A> 
  ~A ~A 
==> 
 +manual> 
  cmd punch 
  finger index 
  hand ~A)"
	      )

(defparameter *template-create-retrieval*
  "
(p ~A 
 ?manual> 
  preparation free 
  processor free 
  execution free 
 ?retrieval> 
  state free 
 ?imaginal> 
  state free 
 ?goal> 
  state free 
 ?visual>
  state free 
 =~A> 
  ~A =X 
==> 
 +~A> 
  kind memory
  ~A =X)"
	      )

(defun create-production-name  (buffer-from slot-from buffer-to slot-to)
   (format nil "from-~A-~A-to-~A-~A" buffer-from slot-from buffer-to slot-to))

(defun create-production-from-template (buffer-from slot-from buffer-to slot-to)
  (let ((name (create-production-name buffer-from slot-from buffer-to slot-to)))
    (cond ((equal buffer-to 'manual)
	   (read-from-string (format nil
				      *template-create-manual*
				      name 
				      buffer-from
				      slot-from
				      'left
				      'left))
	   (read-from-string (format nil
				      *template-create-manual*
				      name 
				      buffer-from
				      slot-from
				      'right
				      'right)))
	  ((equal buffer-to 'retrieval)
	   (read-from-string (format nil
				     *template-create-retrieval*
				     name 
				     buffer-from
				     slot-from
				     buffer-to
				     slot-to)))
	  (t
	   (read-from-string (format nil
			      *template-modify*
			      name
			      buffer-to
			      buffer-from
			      slot-from
			      buffer-to
			      slot-to))))))

    
(defun create-productions-from-template ()
  "Populates a model with all the possible productions"
  (dolist (buffer-from *buffers*)
    (dolist (slot-from *slots*)
      (dolist (buffer-to *buffers*)
	(dolist (slot-to *slots*)
	  (when (and (not (equalp buffer-to 'visual))
		     (not (equalp buffer-from 'manual))
		     (not (equalp buffer-to buffer-from)))
	    (eval (create-production-from-template buffer-from slot-from buffer-to slot-to))))))))


(defun get-production-utility (prod)
  "Retrieves the utility associated with a given production (passed as a symbol)"
  (caar (no-output (spp-fct `(,prod :u)))))


(defun create-utility-matrix ()
  "Creates a matrix of the utilities of transferring the contents between buffers" 
  (let* ((nb (length *buffers*))
	 (ns (length *slots*))
	 (n (* nb ns))
	 (matrix (make-array `(,n ,n) :initial-element "NA")))
    (dolist (buffer-from *buffers*)
      (dolist (slot-from *slots*)
	(dolist (buffer-to *buffers*)
	  (dolist (slot-to *slots*)
	    (when (and (not (equalp buffer-to 'visual))
		       (not (equalp buffer-from 'manual))
		       (not (equalp buffer-to buffer-from)))
	      (let ((prod (read-from-string
			   (create-production-name
			    buffer-from
			    slot-from
			    buffer-to
			    slot-to)))
		    (row (+ (* (position buffer-from *buffers*)
			       ns)
			    (position slot-from *slots*)))
		    (col (+ (* (position buffer-to *buffers*)
			       ns)
			    (position slot-from *slots*))))
		(print prod)
		(setf (aref matrix row col) (production-utility prod))))))))
    matrix))

(defun save-matrix (matrix &optional (filename "./utilities.csv"))
  "Saves a matrix to a CSV file"
  (let* ((dims (array-dimensions matrix))
	 (nrows (first dims))
	 (ncols (second dims)))
    (with-open-file (out filename
			 :direction :output
			 :if-exists :overwrite
			 :if-does-not-exist :create)
      (dotimes (i nrows)
	(dotimes (j ncols)
	  (let ((val (aref matrix i j)))
	    (when (null val)
	      (setf val "NA"))
	    (format out "~3$" val))
	  (if (< j (1- ncols))
	      (format out ",")
	      (format out "~%")))))))
		

(defun save-utilities-rformat (&optional (filename "utilities-tibble.csv"))
  "Creates a matrix of the utilities of transferring the contents between buffers" 
  (with-open-file (out filename :direction :output
				:if-exists :overwrite
				:if-does-not-exist :create)
    (format out "BufferFrom,SlotFrom,BufferTo,SlotTo,Production,Value~%")
    (dolist (buffer-from *buffers*)
      (dolist (slot-from *slots*)
	(dolist (buffer-to *buffers*)
	  (dolist (slot-to *slots*)
	    (let ((prod (read-from-string
			   (create-production-name
			    buffer-from
			    slot-from
			    buffer-to
			    slot-to))))
	      (if (and (not (equalp buffer-to 'visual))
		       (not (equalp buffer-from 'manual))
		       (not (equalp buffer-to buffer-from)))
		  (format out "~A,~A,~A,~A,~A,~A~%"
			  buffer-from slot-from
			  buffer-to slot-to
			  prod
			  (production-utility prod))
		  (format out "~A,~A,~A,~A,~A,NA~%"
			  buffer-from slot-from
			  buffer-to slot-to
			  prod)))))))))
  
;;; -------------------------------------------------------------- ;;;
;;; TEST MODEL: SIMON TASK
;;; -------------------------------------------------------------- ;;;

(define-model simon-train

  (sgp :er t
       :esc t
       :ul t
;;       :ult t
       :egs 0.5
       :alpha 0.05
       :do-not-harvest imaginal
       :do-not-harvet visual
;;       :do-not-harvest retrieval
       )
  
  (chunk-type memory kind memory slot1 slot2 slot3)
  
  (add-dm (memory) (stimulus) (rule) (circle) (square) (simon)
	  (rule1 isa memory kind memory slot1 rule slot2 circle slot3 left)
	  (rule2 isa memory kind memory slot2 rule slot2 square slot3 right)
	  (congruent1 isa memory kind stimulus slot1 circle slot2 left slot3 black)
	  (congruent2 isa memory kind stimulus slot1 square slot2 right slot3 black)
	  (incongruent1 isa memory kind stimulus slot1 circle slot2 right slot3 black)
	  (incongruent2 isa memory kind stimulus slot1 square slot2 left slot3 black)

	  ;; Fillers
	  (simon-goal isa memory kind stimulus slot1 simon slot2 rule)
	  (imaginal-sketchpad isa memory kind memory))
  
  )


(create-productions-from-template)
(install-device '("motor" "keyboard"))

(defparameter *responses* '((square . right)
			    (circle . left))
  "Responses for the Simon task")


(defparameter *trial-start* 0 "The starting time of a trial (in MP-time)")

(defparameter *trial-response* nil "The response given by the model in a trial")

(defparameter *correct-response* nil "The correct response for a trial")

(defparameter *trial-type* nil "The trial type (congruent/incongruent)")

(defparameter *trial-ended* nil "Flag for whether a trial has ended (through response/runout)or not")

(defparameter *max-trial-duration* 5
  "Maximum duration of a trial before it runs out (with negative feedback")

(defparameter *trial-accuracies* () "Accuracies")

(defparameter *trial-rts* () "Response times")

(defun get-correct-response (chunk)
  "Returns the correct response given a stimulus encoded as visual chunk"
  (let ((shape (no-output (chunk-slot-value-fct chunk 'slot1))))
    (cdr (assoc shape *responses*))))

(defun process-response (model key)
  "Processes a model's key press"
  (declare (ignore model))
  (model-output "---> PROCESS RESPOOOOOONSE --->") 
  (if (string-equal key "j")
      (setf *trial-response* 'right)
      (setf *trial-response* 'left))
  (schedule-event-relative 0.001 'reward-response))

(defun end-trial ()
  "Marks the end of a trial"
  (setf *trial-ended* t))

(defun reward-response ()
  "Triggers a reward for a behavioral response"
  (if (equal *trial-response*
	     *correct-response*) 
      (if (equal *trial-type* 'incongruent)
	  (trigger-reward 10)
	  (trigger-reward 10)
	  )
      (trigger-reward -10))
  (schedule-event-relative 0.005 'end-trial))
      

(defun trial-end-p (num)
  "A trial ends either when a response is made or MAX-DURATION is exceeded" 
  (declare (ignore num))
  (or (> (- (mp-time) *trial-start*)
	 *max-trial-duration*)
      *trial-ended*))

(defun reset-buffers ()
  "Resets the slots of Goal and Imaginal to initial conditions"
  (mod-buffer-chunk 'imaginal '(slot1 nil slot2 nil slot3 nil))
  (mod-buffer-chunk 'goal '(slot1 simon slot2 rule slot3 nil))
  (clear-buffer 'retrieval))
  

(add-act-r-command "process-response" 'process-response)

(add-act-r-command "trial-end?" 'trial-end-p)

(monitor-act-r-command "output-key" "process-response")

(defun run-trial ()
  "Runs a single trial with a randomly-geneated stimulus"4
  ;;; Generates a stimulus to place in thge visual buffer
  (let* ((stimulus (pick (list 'congruent1
			       'congruent2
			       'incongruent1
			       'incongruent2)))
	 (response (get-correct-response stimulus))
	 (stimtype (if (member stimulus (list 'congruent1 'congruent2))
		       'congruent
		       'incongruent)))
		       
    ;; Set up trial variables
    (setf *trial-start* (mp-time))
    (setf *trial-response* nil)
    (setf *correct-response* response)
    (setf *trial-type* stimtype)
    (setf *trial-ended* nil)

    (model-output "---> START TRIAL ~A" stimtype)  ;; Trace Marker

    ;; Schedule a negatiev reward if no motor response is made by MAX-DURATION.
    ;; This needs to be done right before the trial ends, otherwise no reward
    ;; will be propagated.
    (schedule-event-relative (- *max-trial-duration*
				0.01)
			     'trigger-reward
			     :module 'utility
			     :params '(-10))
    ;;; Set up the buffers
    
    ;;(goal-focus simon-goal)
    (goal-focus imaginal-sketchpad)
    (set-buffer-chunk 'visual stimulus)
    (set-buffer-chunk 'imaginal 'imaginal-sketchpad)
    (clear-buffer 'retrieval)

    ;;; Run the trial
    (run-until-condition 'trial-end-p)

    ;; Save the trial behavioral data
    (push (- (mp-time) *trial-start*)
	  *trial-rts*)
    (push (if (equal *correct-response* *trial-response*) 1 0)
	  *trial-accuracies*)
    
    ;;; REset: Remove remaining events and change buffers
    (dolist (module '(utility imaginal motor))
      (dolist (id (mp-modules-events module))
	(delete-event id)))
    (model-output "---> TRIAL ENDED, ACC = ~A" (first *trial-accuracies*))
    (reset-buffers)))

(defun save-utilities-rformat (&optional (filename "utilities-tibble.csv"))
  "Saves a tibble of Utility data in long format in a CSV file" 
  (with-open-file (out filename :direction :output
				:if-exists :overwrite
				:if-does-not-exist :create)
    (format out "BufferFrom,SlotFrom,BufferTo,SlotTo,Production,Value~%")
    (dolist (buffer-from *buffers*)
      (dolist (slot-from *slots*)
	(dolist (buffer-to *buffers*)
	  (dolist (slot-to *slots*)
	    (let ((prod (read-from-string
			   (create-production-name
			    buffer-from
			    slot-from
			    buffer-to
			    slot-to))))
	      (if (and (not (equalp buffer-to 'visual))
		       (not (equalp buffer-from 'manual))
		       (not (equalp buffer-to buffer-from)))
		  (format out "~A,~A,~A,~A,~A,~A~%"
			  buffer-from slot-from
			  buffer-to slot-to
			  prod
			  (production-utility prod))
		  (format out "~A,~A,~A,~A,~A,NA~%"
			  buffer-from slot-from
			  buffer-to slot-to
			  prod)))))))))


(defun save-behavioral-data-rformat (&optional (filename "behavioral-data.csv"))
  "Saves a tibble of behavioral data in CSV format"
  (with-open-file (out filename :direction :output
				:if-exists :overwrite
				:if-does-not-exist :create)
    (format out "Trial,Accuracy,RT~%")
    (let ((index 0)
	  (pairs (pairlis *trial-accuracies* *trial-rts*)))
      (dolist (pair pairs)
	(format out "~A,~A,~A~%"
		index
		(car pair)
		(cdr pair))
	(incf index)))))

(defun reset ()
  (setf *trial-accuracies* nil)
  (setf *trial-rts* nil)
  (delete-model)
  (load "pgm-mod2.lisp"))

(defun run-trials (n &optional (reset t))
  (when reset
    (reset))
  (dotimes (i n)
    (run-trial)))
      
