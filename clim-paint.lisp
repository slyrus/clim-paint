
(in-package :clim-paint)

;;;
;;; mutable points
(defclass mutable-point (point)
  ((x :type coordinate :initarg :x :accessor point-x)
   (y :type coordinate :initarg :y :accessor point-y)))

(defmethod point-position ((self mutable-point))
  (with-slots (x y) self
    (values x y)))

(defun make-mutable-point (x y)
  (make-instance 'mutable-point
    :x (coerce x 'coordinate)
    :y (coerce y 'coordinate)))

;;;
;;; mutable lines
(defclass mutable-line (line)
  ((p1 :type point :initarg :p1)
   (p2 :type point :initarg :p2)))

(defun make-mutable-line (start-point end-point)
  (make-instance 'mutable-line :p1 start-point :p2 end-point))

(defun make-mutable-line* (start-x start-y end-x end-y)
  (setf start-x (coerce start-x 'coordinate)
        start-y (coerce start-y 'coordinate)
        end-x (coerce end-x 'coordinate)
        end-y (coerce end-y 'coordinate))
  (if (and (= start-x end-x)
           (= start-y end-y))
      +nowhere+
      (make-mutable-line (make-mutable-point start-x start-y)
                         (make-mutable-point end-x end-y))))

(defmethod line-start-point* ((line mutable-line))
  (with-slots (p1) line
    (with-slots (x y)
        p1
      (values x y))))

(defmethod line-end-point* ((line mutable-line))
  (with-slots (p2) line
    (with-slots (x y)
        p2
      (values x y))))

(defmethod line-start-point ((line mutable-line))
  (with-slots (p1) line
    p1))

(defmethod line-end-point ((line mutable-line))
  (with-slots (p2) line
    p2))

;;;
;;; some special variables to be used for drawing/dragging
(defparameter *highlight-color* +orange+)

(defparameter *drag-color* +green+)

;;
;; clim-paint view
(defclass clim-paint-view (view) ())

;;;
;;; clim-paint frame
(define-application-frame clim-paint ()
  ((shapes :initform (list (make-mutable-point 100 100)) :accessor shapes)
   (ink :initform +blue+ :accessor ink))
  (:menu-bar clim-paint-menubar)
  (:panes
   (app :application
        :display-function #'clim-paint-display
        :default-view (make-instance 'clim-paint-view))
   (interactor :interactor :height 300 :width 600))
  (:layouts
   (default
       (vertically ()
         app
         interactor))))
;;;
;;; points
(defclass point-presentation (standard-presentation) ())

(define-presentation-type point-presentation ()
  :inherit-from 'point)

(define-presentation-method present (point (type point) pane
                                           (view clim-paint-view) &key)
  (with-accessors ((ink ink))
      (pane-frame pane)
    (draw-circle pane point 6 :ink ink :filled t)))

;;;
;;; lines
(defclass line-presentation (standard-presentation) ())

(define-presentation-type line-presentation ()
  :inherit-from 'line)

(define-presentation-method present (line (type line) pane
                                          (view clim-paint-view) &key)
  (with-accessors ((ink ink))
      (pane-frame pane)
    (draw-line pane
               (line-start-point line)
               (line-end-point line)
               :ink ink)))

;;;
;;; HACK ALERT!
;;;
;;; This present* method feels like a hack. It would be great if there
;;; were a way (that I knew of) for embedding these kinds of options in
;;; the default present presentation-method stuff. As it is, I can
;;; dispatch on the class of the object I'm presenting, but I don't
;;; want to have a big switch statement inside my main display
;;; function, so use this hack until I figure out a better mechanism.

(defgeneric present* (object)
  (:method ((point point))
    (present point
             'point
             :record-type 'point-presentation :single-box t))
  (:method ((line line))
    (present line
             'line
             :record-type 'line-presentation :single-box nil)))

;;;
;;; main display function
(defun clim-paint-display (frame pane)
  (declare (ignore pane))
  (with-accessors ((shapes shapes))
      frame
    (mapcar #'present* shapes)))

(defun point+ (p1 p2)
  (multiple-value-bind (x1 y1)
      (point-position p1)
    (multiple-value-bind (x2 y2)
        (point-position p2)
      (make-mutable-point (+ x1 x2) (+ y1 y2)))))

(defun point= (p1 p2)
  (when (and (pointp p1) (pointp p2))
    (multiple-value-bind (x1 y1)
        (point-position p1)
      (multiple-value-bind (x2 y2)
          (point-position p2)
        (and (= x1 x2)
             (= y1 y2))))))

(defun point- (p1 p2)
  (multiple-value-bind (x1 y1)
      (point-position p1)
    (multiple-value-bind (x2 y2)
        (point-position p2)
      (make-mutable-point (- x1 x2) (- y1 y2)))))

(defun point-distance (p1 p2)
  (multiple-value-bind (x1 y1)
      (point-position p1)
    (multiple-value-bind (x2 y2)
        (point-position p2)
      (sqrt (+ (* (- x2 x1) (- x2 x1))
               (* (- y2 y1) (- y2 y1)))))))

(defun square (x)
  (* x x))

(defun point-line-distance (test-point line-point-1 line-point-2)
  (multiple-value-bind (x1 y1)
      (point-position line-point-1)
    (multiple-value-bind (x2 y2)
        (point-position line-point-2)
      (multiple-value-bind (x0 y0)
          (point-position test-point)
        (/ (abs (+ (* (- y2 y1) x0)
                   (* x2 y1)
                   (- (+ (* (- x2 x1) y0)
                         (* y2 x1)))))
           (sqrt (+ (square (- y2 y1))
                    (square (- x2 x1)))))))))

(defun line-point-between-p (test-point line-point-1 line-point-2
                                 &key (line-fuzz 7))
  (let ((distance (point-line-distance test-point line-point-1 line-point-2)))
    (values (< distance line-fuzz)
            distance)))

(defun find-top-level-output-record (record)
  (when record
    (with-accessors ((parent output-record-parent))
        record
      (if (null parent)
          record
          (find-top-level-output-record parent)))))

(define-presentation-method highlight-presentation
    ((type point) (record point-presentation) stream state)
  (let ((point (presentation-object record)))
    (with-accessors ((x point-x)
                     (y point-y))
        point
      (case state
        (:highlight
         (draw-circle stream point 6 :ink *highlight-color* :filled t))
        (:unhighlight (queue-repaint stream
                                     (make-instance 'window-repaint-event
                                                    :sheet stream
                                                    :region (transform-region
                                                             (sheet-native-transformation stream)
                                                             record))))))))

(define-presentation-method highlight-presentation
    ((type line) (record line-presentation) stream state)
  (let ((line (presentation-object record)))
    (with-accessors ((start line-start-point)
                     (end line-end-point))
        line
      (case state
        (:highlight
         (draw-line stream start end
                    :line-thickness 4 :ink *highlight-color*))
        (:unhighlight (queue-repaint stream
                                     (make-instance 'window-repaint-event
                                                    :sheet stream
                                                    :region (transform-region
                                                             (sheet-native-transformation stream)
                                                             record))))))))

(define-presentation-method presentation-refined-position-test
    ((type line) (record line-presentation) x y)
  (let ((line (presentation-object record)))
    (line-point-between-p (make-mutable-point x y)
                          (line-start-point line)
                          (line-end-point line))))

(defun get-pointer-position (pane)
  "Returns a point with x and y values of the stream-pointer-position
of pane."
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-mutable-point x y)))

;;
;; dragging-output doesn't support the feedback arg. No reason it
;; shouldn't so let's take McCLIM's version and make a new version
;; that takes a feedback arg.
(locally
    (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (defmacro dragging-output* ((&optional (stream *standard-output*) &rest args
                                         &key (repaint t)
                                         finish-on-release
                                         multiple-window)
                               &body body)
    (declare (ignore repaint finish-on-release multiple-window feedback))
    (let ((record (gensym "record"))
          (feedback-fn (gensym "feedback-fn"))
          (draw-feedback-fn (gensym "feedback-fn")))
      `(let* ((,record (with-output-to-output-record (,stream)))
              (,draw-feedback-fn
               ,@body)
              (,feedback-fn
               (lambda
                   (record stream initial-x initial-y x y event)
                 (with-accessors ((ink ink)
                                  (shapes shapes))
                     *application-frame*)
                 (multiple-value-bind (record-x record-y)
                     (output-record-position record)
                   #+(or)
                   (let ((erase-final t))
                     (finish-on-release t)
                     (flet ((simple-erase ()
	                      (when erase-final
		                (when (output-record-parent record)
		                  (delete-output-record record (output-record-parent record)))
		                (climi::with-double-buffering
		                    ((stream record) (buffer-rectangle))
		                  (stream-replay stream buffer-rectangle)))))))
	           (let ((dx (- record-x initial-x))
	                 (dy (- record-y initial-y)))
                     (case event
                       (:draw
                        (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2)
	                    record
	                  (when (output-record-parent record)
		            (delete-output-record record (output-record-parent record)))
	                  (setf (output-record-position record)
		                (values (+ dx x) (+  dy y)))
                          (add-output-record (funcall ,draw-feedback-fn stream x y) record)
	                  (stream-add-output-record stream record)
	                  (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
		              record
		            (multiple-value-bind (area-x1 area-y1 area-x2 area-y2)
		                (climi::bound-rectangles old-x1 old-y1 old-x2 old-y2
				                         new-x1 new-y1 new-x2 new-y2)
		              (climi::with-double-buffering
		                  ((stream area-x1 area-y1 area-x2 area-y2)
			           (buffer-rectangle))
		                (stream-replay stream buffer-rectangle))))))
                       (:erase
                        (with-bounding-rectangle* (x1 y1 x2 y2)
		            record
		          (clear-output-record record)
                          (climi::with-double-buffering
		              ((stream x1 y1 x2 y2)
			       (buffer-rectangle))
		            (stream-replay stream buffer-rectangle))))))))))
        (drag-output-record ,stream ,record :erase-final t ,@args :feedback ,feedback-fn)))))

(define-clim-paint-command (com-drag-move-point)
    ((point point))
  (with-accessors ((shapes shapes)
                   (ink ink))
      *application-frame*
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (multiple-value-bind (startx starty)
          (stream-pointer-position pane)
        (multiple-value-bind (x y)
	    (dragging-output*
                (pane :finish-on-release t)
              (lambda (stream x y)
                (flet ((connect-neighbors (point)
                         (let ((neighbors
                                (find-lines-containing point shapes)))
                           (loop for other-line in neighbors
                              do (let ((other-point
                                        (if (eq (line-start-point other-line) point)
                                            (line-end-point other-line)
                                            (line-start-point other-line))))
                                   (with-accessors ((nx1 point-x) (ny1 point-y)) point
                                     (with-accessors ((nx2 point-x) (ny2 point-y)) other-point
                                       (draw-line* stream
                                                   (+ nx1 (- x startx))
                                                   (+ ny1 (- y starty))
                                                   nx2
                                                   ny2
                                                   :line-thickness 4
                                                   :ink *drag-color*))))))))
                  (with-output-to-output-record (stream)
                    (with-accessors ((x1 point-x) (y1 point-y)) point
                      (draw-circle* stream
                                    (+ x1 (- x startx))
                                    (+ y1 (- y starty))
                                    6
                                    :ink ink :filled t)
                      (connect-neighbors point))))))
          (with-accessors ((x1 point-x) (y1 point-y)) point
            (progn
	      (setf x1 (+ x1 (- x startx)))
              (setf y1 (+ y1 (- y starty))))))))))

(define-clim-paint-command (com-move-point)
    ((presentation presentation))
  (with-accessors ((shapes shapes))
      *application-frame*
    (let ((point (presentation-object presentation)))
          (com-drag-move-point point))))

(defun insert-before (new-item before-item list)
  "If before-item is a member of list, inserts new-item in list
 immediately before new-item, otherwise new-item is prepended to the
 beginning of list. Returns the (destructively) modified list."
  (let ((tail (member before-item list)))
            (if tail
                (progn (rplacd tail (cons (car tail) (cdr tail)))
                       (rplaca tail new-item))
                (push new-item list)))
  list)

(defun insert-after (new-item after-item list)
  "If after-item is a member of list Inserts new-item in list
immediately after new-item, otherwise it appends new-item to the end
of list. Returns the (destructively) modified list."
  (let ((tail (member after-item list)))
            (if tail
                (rplacd tail (cons new-item (cdr tail)))
                (append list (list new-item))))
  list)

(define-clim-paint-command (com-add-point :name t)
    ((x real :prompt "X")
     (y real :prompt "Y")
     &key
     (previous-point point))
  (with-accessors ((shapes shapes))
      *application-frame*
    (when (and x y)
      (let ((point (make-mutable-point (max x 0)
                                       (max y 0))))
        (if previous-point
            (progn
              (insert-before point previous-point shapes)
              (com-add-line point previous-point))
            (push point shapes))
        point))))

(define-clim-paint-command (com-add-line :name t)
    ((point1 point :prompt "Point 1")
     (point2 point :prompt "Point 2"))
  (with-accessors ((shapes shapes))
      *application-frame*
    (when (and point1 point2)
      (let ((line (make-mutable-line point1 point2)))
        (push line shapes)))))

(define-clim-paint-command (com-drag-add-point)
    ((old-point t))
  (with-accessors ((ink ink))
      *application-frame*
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (multiple-value-bind (x y)
	  (dragging-output (pane :finish-on-release t)
	    (draw-circle pane (get-pointer-position pane) 6
                         :ink ink :filled t))
        (com-add-point x y :previous-point old-point)))))

(define-gesture-name add-point-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator point-dragging-add-translator
    (point com-drag-add-point clim-paint
           :gesture add-point-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (pointp object)))
    (object)
  (list object))

(define-gesture-name move-point-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-point-translator
    (point com-move-point clim-paint
           :gesture move-point-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (pointp object)))
    (object presentation)
  (list presentation))

(define-clim-paint-command (com-drag-split-line)
    ((line line) (presentation presentation) (frame frame))
  (declare (ignore presentation))
  (with-accessors ((ink ink)
                   (shapes shapes))
      frame
    (let ((pane (get-frame-pane frame 'app)))
      (multiple-value-bind (x y)
	  (dragging-output (pane :finish-on-release t)
	    (draw-circle pane (get-pointer-position pane) 6
                         :ink ink :filled t))
        (let ((p1 (find (line-start-point line) shapes :test 'point=))
              (p2 (find (line-end-point line) shapes :test 'point=)))
          (setf shapes (delete-if (lambda (x)
                                    (and (linep x)
                                         (or
                                          (and (point= (line-start-point x) p1)
                                               (point= (line-end-point x) p2))
                                          (and (point= (line-start-point x) p2)
                                               (point= (line-end-point x) p1)))))
                                  shapes))
          (let ((new-point (com-add-point x y)))
            (com-add-line p1 new-point)
            (com-add-line p2 new-point)))))))

(define-clim-paint-command (com-split-line)
    ((presentation t))
  (let ((line (presentation-object presentation)))
    (com-drag-split-line line presentation *application-frame*)))

(define-gesture-name split-line-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator split-line-translator
    (line com-split-line clim-paint
          :gesture split-line-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))


(defun find-lines-containing (point shapes)
  (loop for shape in shapes
     when (and (linep shape)
               (or (eq point (line-start-point shape))
                   (eq point (line-end-point shape))))
     collect shape))

(define-clim-paint-command (com-drag-move-line)
    ((line line))
  (with-accessors ((shapes shapes)
                   (ink ink))
      *application-frame*
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (multiple-value-bind (startx starty)
          (stream-pointer-position pane)
        (multiple-value-bind (x y)
	    (dragging-output*
                (pane :finish-on-release t)
              (lambda (stream x y)
                (flet ((connect-neighbors (point)
                         (let ((neighbors
                                (remove line (find-lines-containing point shapes))))
                           (loop for other-line in neighbors
                              do (let ((other-point
                                        (if (eq (line-start-point other-line) point)
                                            (line-end-point other-line)
                                            (line-start-point other-line))))
                                   (with-accessors ((nx1 point-x) (ny1 point-y)) point
                                     (with-accessors ((nx2 point-x) (ny2 point-y)) other-point
                                       (draw-line* stream
                                                   (+ nx1 (- x startx))
                                                   (+ ny1 (- y starty))
                                                   nx2
                                                   ny2
                                                   :line-thickness 4
                                                   :ink *drag-color*))))))))
                  (with-output-to-output-record (stream)
                    (let ((p1 (line-start-point line))
                          (p2 (line-end-point line)))
                      (with-accessors ((x1 point-x) (y1 point-y)) p1
                        (with-accessors ((x2 point-x) (y2 point-y)) p2
                          (draw-circle* stream
                                        (+ x1 (- x startx))
                                        (+ y1 (- y starty))
                                        6
                                        :ink ink :filled t)
                          (draw-circle* stream
                                        (+ x2 (- x startx))
                                        (+ y2 (- y starty))
                                        6
                                        :ink ink :filled t)
                          (connect-neighbors p1)
                          (connect-neighbors p2)
                          (draw-line* stream
                                      (+ x1 (- x startx))
                                      (+ y1 (- y starty))
                                      (+ x2 (- x startx))
                                      (+ y2 (- y starty))
                                      :line-thickness 4
                                      :ink *drag-color*))))))))
          (let ((p1 (line-start-point line))
                (p2 (line-end-point line)))
            (with-accessors ((x1 point-x) (y1 point-y)) p1
              (with-accessors ((x2 point-x) (y2 point-y)) p2
                (progn
	          (setf x1 (+ x1 (- x startx)))
                  (setf y1 (+ y1 (- y starty)))
                  (setf x2 (+ x2 (- x startx)))
                  (setf y2 (+ y2 (- y starty))))))))))))

(define-clim-paint-command (com-move-line)
    ((presentation t))
  (with-accessors ((shapes shapes))
      *application-frame*
    (let ((line (presentation-object presentation)))
      (com-drag-move-line line))))

(define-gesture-name move-line-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-line-translator
    (line com-move-line clim-paint
          :gesture move-line-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))


(define-clim-paint-command (com-quit :name t :menu "Quit")
   ()
  (frame-exit *application-frame*))

(make-command-table 'clim-paint-file-command-table
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(make-command-table 'clim-paint-menubar
		    :errorp nil
		    :menu '(("File" :menu clim-paint-file-command-table)))

(defvar *clim-paint-app*)

(defun clim-paint (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'clim-paint)))
             (setf *clim-paint-app* frame)
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "clim-paint")
        (run))))

