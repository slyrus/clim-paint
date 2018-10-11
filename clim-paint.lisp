
(in-package :clim-paint)

(define-application-frame clim-paint ()
  ((shapes :initform (list (list (make-point 100 100))) :accessor shapes)
   (ink :initform +blue+ :accessor ink)
   (view-origin :initform nil :accessor view-origin))
  (:menu-bar clim-paint-menubar)
  (:panes
   (app :application
        :display-function #'clim-paint-display)
   (interactor :interactor :height 300 :width 600))
  (:layouts
   (default
       (vertically ()
         app
         interactor))))

;;
;; points
(defclass point-presentation (standard-presentation) ())

(define-presentation-type point-presentation ()
  :inherit-from 'point)

(define-presentation-method present (point (type point-presentation) pane
                                           view &key)
  (with-accessors ((ink ink)
                   (view-origin view-origin))
      (pane-frame pane)
    (draw-circle pane point 6 :ink ink :filled t)))

;;
;; lines
(defclass line-presentation (standard-presentation) ())

(define-presentation-type line-presentation ()
  :inherit-from 'line)

(define-presentation-method present (line (type line) pane
                                           view &key)
  (with-accessors ((ink ink)
                   (view-origin view-origin))
      (pane-frame pane)
    (draw-line pane
               (line-start-point line)
               (line-end-point line)
               :ink ink)))

;;
;; main display function
(defun clim-paint-display (frame pane)
  (with-accessors ((shapes shapes)
                   (ink ink)
                   (view-origin view-origin))
      frame
    ;;
    ;; now let's draw the points
    (multiple-value-bind (left top)
        (stream-cursor-position pane)
      (setf view-origin (make-point left top)))
    
    (let ((points (car shapes)))
      (loop
         :for previous-point = nil then point
         :for point :in points
         :do
           (present point
                    'point-presentation
                    :record-type 'point-presentation)
           (when previous-point
             (present (make-line previous-point point)
                      'line
                      :record-type 'line-presentation :single-box t))))))

(defun point+ (p1 p2)
  (multiple-value-bind (x1 y1)
      (point-position p1)
    (multiple-value-bind (x2 y2)
        (point-position p2)
      (make-point (+ x1 x2) (+ y1 y2)))))

(defun point= (p1 p2)
  (multiple-value-bind (x1 y1)
      (point-position p1)
    (multiple-value-bind (x2 y2)
        (point-position p2)
      (and (= x1 x2)
           (= y1 y2)))))

(defun point- (p1 p2)
  (multiple-value-bind (x1 y1)
      (point-position p1)
    (multiple-value-bind (x2 y2)
        (point-position p2)
      (make-point (- x1 x2) (- y1 y2)))))

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

(defmethod output-record-refined-position-test ((record line-presentation) x y)
  (let ((top (find-top-level-output-record record)))
    (let ((stream (climi::output-history-stream top)))
      (let ((frame (pane-frame stream)))
        (with-accessors ((view-origin view-origin))
            frame
          (let ((line (presentation-object record)))
            (line-point-between-p (point- (make-point x y) view-origin)
                                  (line-start-point line)
                                  (line-end-point line))))))))

(defmethod highlight-output-record ((record line-presentation) stream state)
  (let ((line (presentation-object record)))
    (with-accessors ((start line-start-point)
                     (end line-end-point))
        line
      (case state
        (:highlight
         (let ((line-vector (point- end start))
               (line-length (point-distance start end))
               (origin (view-origin *application-frame*)))
           (declare (ignore line-vector line-length))
           (draw-line stream
                      (point+ origin start)
                      (point+ origin end)
                      :line-thickness 4 :ink +yellow+)))
        (:unhighlight (queue-repaint stream
                                     (make-instance 'window-repaint-event
                                                    :sheet stream
                                                    :region (transform-region
                                                             (sheet-native-transformation stream)
                                                             record))))))))

(defun get-pointer-position (pane)
  "Returns a point with x and y values of the stream-pointer-position
of pane."
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

(define-clim-paint-command (com-move-point)
    ((point point :prompt "point")
     (x real :prompt "X")
     (y real :prompt "Y"))
  (with-accessors ((shapes shapes))
      *application-frame*
    (let ((points (car shapes)))
      (when (and point x y)
        (let ((tail (member point points)))
          (when tail
            (rplaca tail (make-point (max x 0)
                                     (max y 0)))))))))

(define-clim-paint-command (com-drag-move-point)
    ((presentation t))
  (multiple-value-bind (px py)
      (point-position (view-origin *application-frame*))
    (with-accessors ((ink ink))
        *application-frame*
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (x y)
	    (dragging-output (pane :finish-on-release t)
	      (draw-circle pane (get-pointer-position pane) 6
                           :ink ink :filled t))
          (let ((old-point (presentation-object presentation)))
            (com-move-point old-point (- x px) (- y py))))))))

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
    (let ((points (car shapes)))
      (when (and x y)
        (let ((point (make-point (max x 0)
                                 (max y 0))))
          (if previous-point
              (insert-before point previous-point points)
              (push point points))))
      (setf (car shapes) points))))

(define-clim-paint-command (com-drag-add-point)
    ((old-point t))
  (multiple-value-bind (px py)
      (point-position (view-origin *application-frame*))
    (with-accessors ((ink ink))
        *application-frame*
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (x y)
	    (dragging-output (pane :finish-on-release t)
	      (draw-circle pane (get-pointer-position pane) 6
                           :ink ink :filled t))
          (com-add-point (- x px) (- y py) :previous-point old-point))))))

(define-gesture-name add-point-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator point-dragging-add-translator
    (point-presentation com-drag-add-point clim-paint
           :gesture add-point-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (pointp object)))
    (object)
  (list object))

(define-gesture-name move-point-gesture :pointer-button (:left))

(define-presentation-to-command-translator point-dragging-move-translator
    (point-presentation com-drag-move-point clim-paint
                        :gesture move-point-gesture
                        :menu nil
                        :tester ((object presentation event)
                                 (declare (ignore presentation event))
                                 (pointp object)))
    (object presentation)
  (list presentation))

(define-clim-paint-command (com-split-line)
    ((presentation t))
  (with-accessors ((shapes shapes))
      *application-frame*
    (let ((points (car shapes)))
      (let ((line (presentation-object presentation)))
        (let ((i1 (position (line-start-point line) points :test 'point=))
              (i2 (position (line-end-point line) points :test 'point=)))
          (let ((index (min (or i1 (1- (length points)))
                            (or i2 (1- (length points))))))
            (com-drag-add-point (elt points (1+ index)))))))))

(define-gesture-name click-line-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator click-line-translator
    (line com-split-line clim-paint
          :gesture click-line-gesture
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

