
(in-package :clim-paint)

(defclass paint-object ()
  ((ink :initarg :ink :accessor ink)
   (filled :initarg :filled :accessor filledp)))

;;;
;;; paint points
(defclass paint-point (paint-object)
  ((point :type point :initarg point :accessor %point)))

(defgeneric paint-point-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-point)) t)
  (:documentation "Checking for class paint-point"))

(defmethod point-position ((point paint-point))
  (point-position (%point point)))

(defmethod shared-initialize :after ((point paint-point) slot-names &key x y)
  (setf (%point point) (make-point x y)))

(defun make-paint-point (x y &key ink)
  (apply #'make-instance 'paint-point
         :x (coerce x 'coordinate)
         :y (coerce y 'coordinate)
         (when ink
           `(:ink ,ink))))

;;;
;;; paint lines
(defclass paint-line (paint-object)
  ((p1 :type paint-point :initarg :p1)
   (p2 :type paint-point :initarg :p2)))

(defgeneric paint-line-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-line)) t)
  (:documentation "Checking for class paint-line"))

(defun make-paint-line (start-point end-point &key ink)
  (apply #'make-instance 'paint-line :p1 start-point :p2 end-point
         (when ink `(:ink ,ink))))

(defun make-paint-line* (start-x start-y end-x end-y &key ink)
  (setf start-x (coerce start-x 'coordinate)
        start-y (coerce start-y 'coordinate)
        end-x (coerce end-x 'coordinate)
        end-y (coerce end-y 'coordinate))
  (if (and (= start-x end-x)
           (= start-y end-y))
      +nowhere+
      (apply #'make-paint-line
             (apply #'make-paint-point
                    start-x start-y
                    (when ink
                      `(:ink ,ink)))
             (apply #'make-paint-point
                    end-x end-y
                    (when ink
                      `(:ink ,ink)))
             (when ink
               `(:ink ,ink)))))

(defmethod line-start-point* ((line paint-line))
  (with-slots (p1) line
    (with-slots (x y)
        p1
      (values x y))))

(defmethod line-end-point* ((line paint-line))
  (with-slots (p2) line
    (with-slots (x y)
        p2
      (values x y))))

(defmethod line-start-point ((line paint-line))
  (with-slots (p1) line
    p1))

(defmethod line-end-point ((line paint-line))
  (with-slots (p2) line
    p2))

;;;
;;; rectangle
(defclass paint-rectangle (paint-object)
  ((rectangle :type rectangle :initarg rectangle :accessor %rectangle)))

(defmethod shared-initialize :after ((rectangle paint-rectangle) slot-names &key x1 y1 x2 y2)
  (setf (%rectangle rectangle) (make-rectangle* x1 y1 x2 y2)))

(defgeneric paint-rectangle-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-rectangle)) t)
  (:documentation "Checking for class paint-rectangle"))

(defun make-paint-rectangle (x1 y1 x2 y2 &key (ink nil ink-supplied-p)
                                              (filled nil filled-supplied-p))
  (apply #'make-instance 'paint-rectangle
         :x1 (coerce x1 'coordinate)
         :y1 (coerce y1 'coordinate)
         :x2 (coerce x2 'coordinate)
         :y2 (coerce y2 'coordinate)
         (append
          (when ink-supplied-p
            `(:ink ,ink))
          (when filled-supplied-p
            `(:filled ,filled)))))

(defmethod bounding-rectangle* ((object paint-rectangle))
  (bounding-rectangle* (%rectangle object)))


;;;
;;; we want an ellipse object that we can modify but the
;;; standard-ellipse is impaint and itself is a subclass of
;;; elliptical-thing. The problem with elliptical-thiing is that it
;;; stores the transformation of the unit circle to the ellipse, not
;;; the underlying parameters used to define the ellipse via
;;; make-ellipse. So, we'll store those parameters here, along with a
;;; clim:ellipse (presumably a standard-ellipse) that gets regenerated
;;; when the ellipse paramters change.
(defclass paint-ellipse (ellipse paint-object)
  ((center-point :initarg :center-point :accessor center-point)
   (radius-1-dx :initarg :radius-1-dx :accessor radius-1-dx)
   (radius-1-dy :initarg :radius-1-dy :accessor radius-1-dy)
   (radius-2-dx :initarg :radius-2-dx :accessor radius-2-dx)
   (radius-2-dy :initarg :radius-2-dy :accessor radius-2-dy)
   (start-angle :initarg :start-angle :accessor start-angle :initform 0)
   (end-angle :initarg :end-angle :accessor end-angle :initform (* pi 2))
   (filled :initarg :filled :accessor filledp :initform nil)))

(defgeneric paint-ellipse-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-ellipse)) t)
  (:documentation "Checking for class paint-ellipse"))

(defun make-paint-ellipse (center-point
		           radius-1-dx radius-1-dy
		           radius-2-dx radius-2-dy
		           &key (start-angle nil start-angle-supplied-p)
                                (end-angle nil end-angle-supplied-p)
                                (ink nil ink-supplied-p)
                                (filled nil filled-supplied-p))
  (apply #'make-instance 'paint-ellipse
                 :center-point center-point
                 :radius-1-dx radius-1-dx
                 :radius-1-dy radius-1-dy
	         :radius-2-dx radius-2-dx
                 :radius-2-dy radius-2-dy
                 (append
                  (when start-angle-supplied-p `(:start-angle ,start-angle))
                  (when end-angle-supplied-p `(:end-angle ,end-angle))
                  (when ink-supplied-p `(:ink , ink))
                  (when filled-supplied-p `(:filled ,filled)))))

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
  ((shapes :initform (list (make-paint-point 10 20 :ink +red+)
                           (make-paint-point 30 20 :ink +green+)
                           (make-paint-point 50 20 :ink +blue+)
                           (make-paint-rectangle 50 160 100 200 :ink +pink+ :filled t)
                           (make-paint-ellipse (make-paint-point 100 100 :ink +orange+)
                                               10 30 40 15
                                               :ink +orange+
                                               :filled t)
                           (make-paint-ellipse (make-paint-point 250 100 :ink +orange+)
                                               30 0 0 40
                                               :ink +brown+
                                               :filled t))
           :accessor shapes)
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
  :inherit-from 'paint-point)

(define-presentation-method present (object (type paint-point) pane
                                            (view clim-paint-view) &key)
  (multiple-value-bind (x y)
      (point-position object)
    (with-accessors ((ink ink))
        object
      (draw-circle* pane x y 6 :ink ink :filled t))))

;;;
;;; lines
(defclass line-presentation (standard-presentation) ())

(define-presentation-type line-presentation ()
  :inherit-from 'paint-line)

(define-presentation-method present (line (type paint-line) pane
                                          (view clim-paint-view) &key)
  (with-accessors ((ink ink))
      line
    (draw-line pane
               (line-start-point line)
               (line-end-point line)
               :ink ink)))

;;;
;;; rectangles
(defclass rectangle-presentation (standard-presentation) ())

(define-presentation-type rectangle-presentation ()
  :inherit-from 'paint-rectangle)

(define-presentation-method present (rectangle (type paint-rectangle) pane
                                          (view clim-paint-view) &key)
  (with-accessors ((ink ink)
                   (filled filledp))
      rectangle
    (multiple-value-bind (x1 y1 x2 y2)
        (bounding-rectangle* rectangle)
      (draw-rectangle* pane
                       x1 y1 x2 y2
                       :ink ink
                       :filled filled))))

;;;
;;; ellipses
(defclass ellipse-presentation (standard-presentation) ())

(define-presentation-type ellipse-presentation ()
  :inherit-from 'paint-ellipse)

(define-presentation-method present (ellipse (type paint-ellipse) pane
                                          (view clim-paint-view) &key)
  (with-accessors ((center-point center-point)
                   (radius-1-dx radius-1-dx)
                   (radius-1-dy radius-1-dy)
                   (radius-2-dx radius-2-dx)
                   (radius-2-dy radius-2-dy)
                   (start-angle start-angle)
                   (end-angle end-angle)
                   (filledp filledp)
                   (ink ink))
      ellipse
    (with-accessors ((point %point))
        center-point
      (draw-ellipse pane
                    point
                    radius-1-dx radius-1-dy
                    radius-2-dx radius-2-dy
                    :start-angle start-angle
                    :end-angle end-angle
                    :filled filledp
                    :ink ink))))

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
  (:method ((object paint-point))
    (present object
             'paint-point
             :record-type 'point-presentation :single-box t))
  (:method ((object paint-line))
    (present object
             'paint-line
             :record-type 'line-presentation :single-box nil))
  (:method ((object paint-rectangle))
    (present object
             'paint-rectangle
             :record-type 'rectangle-presentation :single-box nil))
  (:method ((object paint-ellipse))
    (present object
             'paint-ellipse
             :record-type 'ellipse-presentation :single-box nil)))

;;;
;;; main display function
(defun clim-paint-display (frame pane)
  (declare (ignore pane))
  (with-accessors ((shapes shapes))
      frame
    (mapcar #'present* shapes)))

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
    ((type paint-point) (record point-presentation) stream state)
  (let ((paint-point (presentation-object record)))
    (multiple-value-bind (x y)
        (point-position paint-point)
      (case state
        (:highlight
         (draw-circle* stream x y 6 :ink *highlight-color* :filled t))
        (:unhighlight
         (queue-repaint
          stream
          (make-instance 'window-repaint-event
                         :sheet stream
                         :region (transform-region
                                  (sheet-native-transformation stream)
                                  record))))))))

(define-presentation-method highlight-presentation
    ((type paint-line) (record line-presentation) stream state)
  (let ((line (presentation-object record)))
    (with-accessors ((start line-start-point)
                     (end line-end-point))
        line
      (case state
        (:highlight
         (draw-line stream start end
                    :line-thickness 4 :ink *highlight-color*))
        (:unhighlight
         (queue-repaint
          stream
          (make-instance 'window-repaint-event
                         :sheet stream
                         :region (transform-region
                                  (sheet-native-transformation stream)
                                  record))))))))

(define-presentation-method presentation-refined-position-test
    ((type paint-line) (record line-presentation) x y)
  (let ((line (presentation-object record)))
    (line-point-between-p (make-point x y)
                          (line-start-point line)
                          (line-end-point line))))

(defun get-pointer-position (pane)
  "Returns a point with x and y values of the stream-pointer-position
of pane."
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

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
                 (with-accessors ((shapes shapes))
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
    ((paint-point paint-point))
  (with-accessors ((shapes shapes))
      *application-frame*
    (with-accessors ((ink ink))
      paint-point
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (startx starty)
            (stream-pointer-position pane)
          (multiple-value-bind (x y)
              (dragging-output*
                  (pane :finish-on-release t)
                (lambda (stream x y)
                  (flet ((connect-neighbors (paint-point)
                           (let ((neighbors
                                  (find-lines-containing paint-point shapes)))
                             (loop for other-line in neighbors
                                do (let ((other-paint-point
                                          (if (eq (line-start-point other-line) paint-point)
                                              (line-end-point other-line)
                                              (line-start-point other-line))))
                                     (multiple-value-bind (nx1 ny1)
                                         (point-position paint-point)
                                       (multiple-value-bind (nx2 ny2)
                                           (point-position other-paint-point)
                                         (draw-line* stream
                                                     (+ nx1 (- x startx))
                                                     (+ ny1 (- y starty))
                                                     nx2
                                                     ny2
                                                     :line-thickness 4
                                                     :ink *drag-color*))))))))
                    (with-output-to-output-record (stream)
                      (multiple-value-bind (x1 y1)
                          (point-position paint-point)
                        (draw-circle* stream
                                      (+ x1 (- x startx))
                                      (+ y1 (- y starty))
                                      6
                                      :ink ink :filled t)
                        (connect-neighbors paint-point))))))
            ;; FIXME! probably want a better API here
            (with-accessors ((point %point)) paint-point
              (with-accessors ((x1 point-x) (y1 point-y) (%p %point)) point
                (setf point (make-point (+ x1 (- x startx))
                                        (+ y1 (- y starty))))))))))))

(define-clim-paint-command (com-move-point)
    ((presentation presentation))
  (let ((point (presentation-object presentation)))
    (com-drag-move-point point)))

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
     (previous-point point)
     (ink color))
  (with-accessors ((shapes shapes)
                   (default-ink ink))
      *application-frame*
    (when (and x y)
      (let ((point (make-paint-point (max x 0)
                                     (max y 0)
                                     :ink (or ink default-ink))))
        (if previous-point
            (let ((line-ink (or ink
                                (ink previous-point)
                                default-ink)))
              (insert-before point previous-point shapes)
              (apply #'com-add-line point previous-point
                     (when ink `(:ink ,line-ink))))
            (push point shapes))
        point))))

(define-clim-paint-command (com-add-line :name t)
    ((point1 point :prompt "Point 1")
     (point2 point :prompt "Point 2")
     &key
     (ink color))
  (with-accessors ((shapes shapes)
                   (default-ink ink))
      *application-frame*
    (when (and point1 point2)
      (let ((line (make-paint-line point1 point2 :ink (or ink default-ink))))
        (push line shapes)))))

(define-clim-paint-command (com-drag-add-point)
    ((old-point t))
  (with-accessors ((ink ink))
      *application-frame*
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (multiple-value-bind (x y)
          (dragging-output (pane :finish-on-release t)
            (draw-circle pane (get-pointer-position pane) 6 :ink ink :filled t))
        (let ((ink (when old-point (ink old-point))))
          (apply #'com-add-point x y
                 (append
                  (when old-point `(:previous-point ,old-point))
                  (when ink `(:ink ,ink)))))))))

(define-gesture-name add-point-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator point-dragging-add-translator
    (paint-point com-drag-add-point clim-paint
                 :gesture add-point-gesture
                 :menu nil
                 :tester ((object presentation event)
                          (declare (ignore presentation event))
                          (paint-point-p object)))
    (object)
  (list object))

(define-gesture-name move-point-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-point-translator
    (paint-point com-move-point clim-paint
           :gesture move-point-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-point-p object)))
    (object presentation)
  (list presentation))

(defun paint-point= (p1 p2)
  (when (and (paint-point-p p1) (paint-point-p p2))
    (multiple-value-bind (x1 y1)
        (point-position p1)
      (multiple-value-bind (x2 y2)
          (point-position p2)
        (and (= x1 x2)
             (= y1 y2))))))

(define-clim-paint-command (com-drag-split-line)
    ((line paint-line) (presentation presentation) (frame frame))
  (declare (ignore presentation))
  (with-accessors ((ink ink)
                   (shapes shapes))
      frame
    (let ((pane (get-frame-pane frame 'app)))
      (multiple-value-bind (x y)
          (dragging-output (pane :finish-on-release t)
            (draw-circle pane (get-pointer-position pane) 6
                         :ink ink :filled t))
        (let ((p1 (find (line-start-point line) shapes :test 'paint-point=))
              (p2 (find (line-end-point line) shapes :test 'paint-point=)))
          (setf shapes (delete-if (lambda (x)
                                    (and (linep x)
                                         (or
                                          (and (paint-point= (line-start-point x) p1)
                                               (paint-point= (line-end-point x) p2))
                                          (and (paint-point= (line-start-point x) p2)
                                               (paint-point= (line-end-point x) p1)))))
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
    (paint-line com-split-line clim-paint
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
    ((line paint-line))
  (with-accessors ((shapes shapes))
      *application-frame*
    (with-accessors ((ink ink))
        line
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (startx starty)
            (stream-pointer-position pane)
          (multiple-value-bind (x y)
              (dragging-output*
                  (pane :finish-on-release t)
                (lambda (stream x y)
                  (flet ((connect-neighbors (paint-point)
                           (let ((neighbors
                                  (remove line (find-lines-containing paint-point shapes))))
                             (loop for other-line in neighbors
                                do (let ((other-paint-point
                                          (if (eq (line-start-point other-line) paint-point)
                                              (line-end-point other-line)
                                              (line-start-point other-line))))
                                     (multiple-value-bind (nx1 ny1)
                                         (point-position paint-point)
                                       (multiple-value-bind (nx2 ny2)
                                           (point-position other-paint-point)
                                         (draw-line* stream
                                                     (+ nx1 (- x startx))
                                                     (+ ny1 (- y starty))
                                                     nx2
                                                     ny2
                                                     :line-thickness 4
                                                     :ink *drag-color*))))))))
                    (with-output-to-output-record (stream)
                      (let ((paint-point-1 (line-start-point line))
                            (paint-point-2 (line-end-point line)))
                        (multiple-value-bind (x1 y1)
                            (point-position paint-point-1)
                          (multiple-value-bind (x2 y2)
                              (point-position paint-point-2)
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
                            (connect-neighbors paint-point-1)
                            (connect-neighbors paint-point-2)
                            (draw-line* stream
                                        (+ x1 (- x startx))
                                        (+ y1 (- y starty))
                                        (+ x2 (- x startx))
                                        (+ y2 (- y starty))
                                        :line-thickness 4
                                        :ink *drag-color*))))))))
            (let ((paint-point-1 (line-start-point line))
                  (paint-point-2 (line-end-point line)))
              ;; FIXME! probably want a better API here
              (with-accessors ((p1 %point)) paint-point-1
                (with-accessors ((x1 point-x) (y1 point-y)) p1
                  (setf p1 (make-point (+ x1 (- x startx))
                                       (+ y1 (- y starty))))
                  (with-accessors ((p2 %point)) paint-point-2
                    (with-accessors ((x2 point-x) (y2 point-y)) p2
                      (setf p2 (make-point (+ x2 (- x startx))
                                           (+ y2 (- y starty)))))))))))))))

(define-clim-paint-command (com-move-line)
    ((presentation t))
  (with-accessors ((shapes shapes))
      *application-frame*
    (let ((line (presentation-object presentation)))
      (com-drag-move-line line))))

(define-gesture-name move-line-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-line-translator
    (paint-line com-move-line clim-paint
          :gesture move-line-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))


(define-clim-paint-command (com-quit :name t :menu "Quit")
    ()
  (frame-exit *application-frame*))

(define-clim-paint-command (com-export-to-pdf :name t :menu "Export to PDF")
    ((pdf-pathname pathname
                   :default *default-pathname-defaults* :insert-default t))
  (let* ((frame *application-frame*)
         (pane (find-pane-named frame 'app))
         (device-type :a4))
    (with-open-file (file-stream pdf-pathname :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (clim-pdf:with-output-to-pdf-stream
          (stream file-stream
                  :header-comments '(:title "clim-paint")
                  :scale-to-fit t
                  :device-type device-type)
        (setf (stream-default-view stream)
              (or (stream-default-view pane)
                  (make-instance 'clim-paint-view)))
        (break)
        (let ((*standard-output* stream))
          (with-accessors ((shapes shapes))
              frame
            (mapcar #'present* shapes)))))))

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

