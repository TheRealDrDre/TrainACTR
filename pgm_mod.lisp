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
	  ((and (> start end)
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
	      "(p ~A ?manual> preparation free processor free execution free ?~A> state free buffer full ?~A> state free buffer full =~A> =~A> ~A =X ==> =~A> ~A =X)"
	      )

(defparameter *template-create-manual*
	      "(p ~A ?~A> state free buffer full ?~A> preparation free processor free execution free =~A> ~A ~A ==> +manual> cmd punch finger index hand ~A)"
	      )

(defparameter *template-create-non-manual*
	      "(p ~A ?manual> preparation free processor free execution free ?~A> state free buffer full ?~A> state free =~A> ~A =X ==> +~A> ~A =X)"
	      )

(defun create-production-from-template (buffer-from slot-from buffer-to slot-to create)
  (let ((name (format nil "~A~A-~A~A~A" buffer-from slot-from buffer-to slot-to create)))
    (cond ((and create
		 (equal buffer-to 'manual))
	   (read-from-string (format nil
				      *template-create-manual*
				      name 
				      buffer-from
				      buffer-to
				      buffer-from
				      slot-from
				      'left
				      'left))
	   (read-from-string (format nil
				       *template-create-manual*
				       name 
				       buffer-from
				       buffer-to
				       buffer-from
				       slot-from
				       'right
				       'right)))
	  ((and create
		(not (equal buffer-to 'manual)))
	   (read-from-string (format nil
				     *template-create-non-manual*
				     name 
				     buffer-from
				     buffer-to
				     buffer-from
				     slot-from
				     buffer-to
				     slot-to)))
	  ((not create)
	   (read-from-string (format nil
			      *template-modify*
			      name 
			      buffer-from
			      buffer-to
			      buffer-to
			      buffer-from
			      slot-from
			      buffer-to
			      slot-to))))))

    
(defun create-productions-from-template ()
  (dolist (buffer-from *buffers*)
    (dolist (slot-from *slots*)
      (dolist (buffer-to *buffers*)
	(dolist (slot-to *slots*)
	  (dolist (create '(t nil))
	    (when (and (not (equalp buffer-to 'visual))
		       (not (equalp buffer-from 'manual))
		       (not (equalp buffer-to buffer-from))
		       (not (and (not create)
				 (equalp buffer-to 'manual)))
		       (not (and (not create)
				 (equalp buffer-to 'retrieval))))
	      (eval (create-production-from-template buffer-from slot-from buffer-to slot-to create)))))))))

;;; -------------------------------------------------------------- ;;;
;;; TEST MODEL: SIMON TASK
;;; -------------------------------------------------------------- ;;;

(define-model test

  (sgp :er t
       :esc t
       :ul t
       :esc 0.2)
  
  (chunk-type memory slot1 slot2 slot3)
  (add-dm (rule) (circle) (square) (simon)
	  (rule1 isa memory slot1 rule slot2 circle slot3 left)
	  (rule2 isa memory slot2 rule slot2 square slot3 right)
	  (congruent1 isa memory slot1 circle slot2 left slot3 black)
	  (congruent2 isa memory slot1 square slot2 right slot3 black)
	  (incongruent1 isa memory slot1 circle slot2 right slot3 black)
	  (incongruent2 isa memory slot1 square slot2 left slot3 black)
	  (simon-goal isa memory slot1 simon slot2 rule))
  )

(create-productions-from-template)

(defparameter *trial-start* 0)

(defparameter *trial-response* nil)

(defparameter *responses* '((square . right)
			    (circle . left)))

(defparameter *max-trial-duration* 10)

(defun get-correct-response (chunk)
  (let ((shape (no-output (chunk-slot-value-fct chunk 'slot1))))
    (cdr (assoc shape *responses*))))

(defun process-response (model key)
  (declare (ignore model))
  (if (string-equal key "j")
      (setf *trial-response* 'right)
      (setf *trial-response* 'left)))

(defun trial-end-p (num)
  (declare (ignore num))
  (or (> (- (mp-time) *trial-start*)
	 *max-trial-duration*)
      *trial-response*))

(add-act-r-command "process-response" 'process-response)
(add-act-r-command "trial-end?" 'trial-end-p)
(monitor-act-r-command "output-key" "process-response")

(defun run-trial ()
  (let* ((stimulus (pick (list 'congruent1 'congruent2 'incongruent1 'incongruent2)))
	 (response (get-correct-response stimulus)))
    (setf *trial-start* (mp-time))
    (setf *trial-response* nil)
    (goal-focus simon-goal)
    (set-buffer-chunk 'visual stimulus)
    (run-until-condition 'trial-end-p)
    (if (equal *trial-response* response)
	(trigger-reward 10)
	(trigger-reward -10))))
