
(in-package :clim-paint)

(defparameter *ellipse-highlight-margin* 6)
(defparameter *ellipse-highlight-thickness* 2)

;;;
;;; we want an ellipse object that we can modify but the
;;; standard-ellipse is immutable (?) and itself is a subclass of
;;; elliptical-thing. The problem with elliptical-thing is that it
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
   (line-thickness :initarg :line-thickness :accessor line-thickness :initform 1)
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
                                (filled nil filled-supplied-p)
                                (line-thickness nil line-thickness-supplied-p))
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
                  (when filled-supplied-p `(:filled ,filled))
                  (when line-thickness-supplied-p `(:line-thickness ,line-thickness)))))

(defclass ellipse-radius-handle-point (selection-handle-point)
  ((radius-index :initarg :radius-index :accessor radius-index
                 :documentation "Either 1 or 2 indicating whether this
                 point corresponds to ellipse radius-1 or radius-2.")
   (radius-dx :initarg :radius-dx :accessor radius-dx)
   (radius-dy :initarg :radius-dy :accessor radius-dy)))

(defclass ellipse-center-handle-point (selection-handle-point)
  ((center-point :initarg :center-point :accessor center-point)))

(defparameter *ellipse-selection-width* 8)

(defun draw-ellipse-selection (pane ellipse &key (ink *selection-color*)
                                                 (radius 5)
                                                 (filled nil))
  (with-accessors ((center-point center-point)
                   (radius-1-dx radius-1-dx)
                   (radius-1-dy radius-1-dy)
                   (radius-2-dx radius-2-dx)
                   (radius-2-dy radius-2-dy)
                   (start-angle start-angle)
                   (end-angle end-angle)
                   (line-thickness line-thickness)
                   (filledp filledp))
      ellipse
    (multiple-value-bind (x1 y1)
        (point-position center-point)
      (let ((theta1 (phase (complex radius-1-dy radius-1-dx)))
            (theta2 (phase (complex radius-2-dy radius-2-dx))))
        (draw-ellipse* pane
                       x1 y1
                       (+ radius-1-dx (* *ellipse-highlight-margin*
                                         (sin theta1)))
                       (+ radius-1-dy (* *ellipse-highlight-margin*
                                         (cos theta1)))
                       (+ radius-2-dx (* *ellipse-highlight-margin*
                                         (sin theta2)))
                       (+ radius-2-dy (* *ellipse-highlight-margin*
                                         (cos theta2)))
                       :ink *selection-color*
                       :line-thickness 2
                       :line-dashes t
                       :filled nil)

        (present (make-instance 'ellipse-center-handle-point
                                :paint-object ellipse
                                :center-point center-point
                                :point center-point
                                :ink ink
                                :radius radius
                                :filled filled)
                 'ellipse-center-handle-point
                 :single-box t)
        (draw-line* pane
                    x1 y1
                    (+ x1 radius-1-dx)
                    (+ y1 radius-1-dy)
                    :ink *selection-color*
                    :line-thickness 3)
        (present (make-instance 'ellipse-radius-handle-point
                                :paint-object ellipse
                                :radius-index 1
                                :radius-dx radius-1-dx
                                :radius-dy radius-1-dy
                                :point (multiple-value-bind (cx cy)
                                           (point-position center-point)
                                         (make-point (+ cx radius-1-dx) (+ cy radius-1-dy)))
                                :ink ink
                                :radius radius
                                :filled filled)
                 'ellipse-radius-handle-point
                 :single-box t)
        (draw-line* pane
                    x1 y1
                    (+ x1 radius-2-dx)
                    (+ y1 radius-2-dy)
                    :ink *selection-color*
                    :line-thickness 3)
        (present (make-instance 'ellipse-radius-handle-point
                                :paint-object ellipse
                                :radius-index 2
                                :radius-dx radius-2-dx
                                :radius-dy radius-2-dy
                                :point (multiple-value-bind (cx cy)
                                           (point-position center-point)
                                         (make-point (+ cx radius-2-dx) (+ cy radius-2-dy)))
                                :ink ink
                                :radius radius
                                :filled filled)
                 'ellipse-radius-handle-point
                 :single-box t)))))

(define-presentation-method present (ellipse (type paint-ellipse) pane
                                             (view clim-paint-view) &key)
  (with-accessors ((center-point center-point)
                   (radius-1-dx radius-1-dx)
                   (radius-1-dy radius-1-dy)
                   (radius-2-dx radius-2-dx)
                   (radius-2-dy radius-2-dy)
                   (start-angle start-angle)
                   (end-angle end-angle)
                   (line-thickness line-thickness)
                   (filledp filledp)
                   (ink ink))
      ellipse
    (multiple-value-bind (x1 y1)
        (point-position center-point)
      (apply #'draw-ellipse* pane
             x1 y1
             radius-1-dx radius-1-dy
             radius-2-dx radius-2-dy
             :filled filledp
             :ink ink
             (append
              (when start-angle `(:start-angle ,start-angle))
              (when end-angle `(:end-angle ,end-angle))
              (when line-thickness `(:line-thickness ,line-thickness)))))
    (if (gethash ellipse (selected-object-hash *application-frame*))
        (draw-ellipse-selection pane ellipse))))

;;;
;;; refined-position test
;;;
;;; In theory we could check to see if this is a filled ellipse and,
;;; if not, only hit the ellipse if we're on the perimeter, but for
;;; the moment we don't do that.
(define-presentation-method presentation-refined-position-test
    ((type paint-ellipse) record x y)
  (let ((ellipse (presentation-object record)))
    (with-accessors ((center-point center-point)
                     (radius-1-dx radius-1-dx)
                     (radius-1-dy radius-1-dy)
                     (radius-2-dx radius-2-dx)
                     (radius-2-dy radius-2-dy)
                     (start-angle start-angle)
                     (end-angle end-angle))
        ellipse
      (multiple-value-bind (center-x center-y)
          (point-position center-point)
        (region-contains-position-p
         (make-ellipse* center-x center-y
                        radius-1-dx radius-1-dy
                        radius-2-dx radius-2-dy
                        :start-angle start-angle
                        :end-angle end-angle)
         x y)))))

;;;
;;; highlighting
(define-presentation-method highlight-presentation
    ((type paint-ellipse) record stream state)
  (let ((paint-ellipse (presentation-object record)))
    (case state
      (:highlight
       (with-accessors ((center-point center-point)
                        (radius-1-dx radius-1-dx)
                        (radius-1-dy radius-1-dy)
                        (radius-2-dx radius-2-dx)
                        (radius-2-dy radius-2-dy)
                        (start-angle start-angle)
                        (end-angle end-angle)
                        (filledp filledp)
                        (ink ink))
           paint-ellipse
         (multiple-value-bind (x1 y1)
             (point-position center-point)
           (let ((theta1 (phase (complex radius-1-dy radius-1-dx)))
                 (theta2 (phase (complex radius-2-dy radius-2-dx))))
             (draw-ellipse* stream
                            x1 y1
                            (+ radius-1-dx (* *ellipse-highlight-margin*
                                              (sin theta1)))
                            (+ radius-1-dy (* *ellipse-highlight-margin*
                                              (cos theta1)))
                            (+ radius-2-dx (* *ellipse-highlight-margin*
                                              (sin theta2)))
                            (+ radius-2-dy (* *ellipse-highlight-margin*
                                              (cos theta2)))
                            :line-thickness *ellipse-highlight-thickness*
                            :start-angle start-angle
                            :end-angle end-angle
                            :ink *highlight-color*
                            :filled nil)))))
      (:unhighlight
       (queue-repaint
        stream
        (make-instance
         'window-repaint-event
         :sheet stream
         :region (transform-region
                  (sheet-native-transformation stream)
                  (with-accessors ((center-point center-point)
                                   (radius-1-dx radius-1-dx)
                                   (radius-1-dy radius-1-dy)
                                   (radius-2-dx radius-2-dx)
                                   (radius-2-dy radius-2-dy))
                      paint-ellipse
                    (multiple-value-bind (x1 y1)
                        (point-position center-point)
                      (make-ellipse* x1 y1
                                     (+ radius-1-dx *ellipse-highlight-margin* 1)
                                     (+ radius-1-dy *ellipse-highlight-margin* 1)
                                     (+ radius-2-dx *ellipse-highlight-margin* 1)
                                     (+ radius-2-dy *ellipse-highlight-margin* 1)))))))))))

;;;
;;; dragging / moving
(defmethod move-dragging ((ellipse paint-ellipse) stream dx dy)
  (with-output-to-output-record (stream)
    (with-accessors ((center-point center-point)
                     (radius-1-dx radius-1-dx)
                     (radius-1-dy radius-1-dy)
                     (radius-2-dx radius-2-dx)
                     (radius-2-dy radius-2-dy)
                     (start-angle start-angle)
                     (end-angle end-angle)
                     (filledp filledp)
                     (ink ink)
                     (line-thickness line-thickness))
        ellipse
      (multiple-value-bind (x1 y1)
          (point-position center-point)
        (draw-ellipse* stream (+ x1 dx) (+ y1 dy)
                       radius-1-dx radius-1-dy
                       radius-2-dx radius-2-dy
                       :start-angle start-angle
                       :end-angle end-angle
                       :ink ink
                       :filled filledp
                       :line-thickness line-thickness)))))

(defmethod move-update ((ellipse paint-ellipse) dx dy)
  (with-accessors ((center-point center-point))
      ellipse
    (multiple-value-bind (x1 y1)
        (point-position center-point)
      (setf center-point (make-point
                          (+ x1 dx)
                          (+ y1 dy))))))


;;;
;;; selection handle dragging / moving

;;; ellipse-center handle
(defmethod move-dragging ((ellipse-center-handle-point ellipse-center-handle-point) stream dx dy)
  (with-accessors ((ellipse paint-object))
      ellipse-center-handle-point
    (with-accessors ((ink ink)
                     (filled filledp))
        ellipse
      (with-output-to-output-record (stream)
        ;; draw the ellipse in its new form
        (with-accessors ((center-point center-point)
                         (radius-1-dx radius-1-dx)
                         (radius-1-dy radius-1-dy)
                         (radius-2-dx radius-2-dx)
                         (radius-2-dy radius-2-dy)
                         (start-angle start-angle)
                         (end-angle end-angle)
                         (filledp filledp)
                         (ink ink)
                         (line-thickness line-thickness))
            ellipse
          (multiple-value-bind (x1 y1)
              (point-position center-point)
            (let ((x1 (+ x1 dx))
                  (y1 (+ y1 dy))
                  (radius-1-dx (- radius-1-dx dx))
                  (radius-1-dy (- radius-1-dy dy))
                  (radius-2-dx (- radius-2-dx dx))
                  (radius-2-dy (- radius-2-dy dy)))
              (draw-ellipse* stream
                             x1 y1
                             radius-1-dx
                             radius-1-dy
                             radius-2-dx
                             radius-2-dy
                             :start-angle start-angle
                             :end-angle end-angle
                             :ink ink
                             :filled filled
                             :line-thickness line-thickness)
              (draw-circle* stream
                            x1 y1
                            5
                            :ink *selection-color*
                            :line-thickness 2)
              (draw-line* stream
                          x1 y1
                          (+ x1 radius-1-dx) (+ y1 radius-1-dy)
                          :ink *selection-color*
                          :line-thickness 3)
              (draw-circle* stream
                            (+ x1 radius-1-dx) (+ y1 radius-1-dy)
                            5
                            :ink *selection-color*
                            :line-thickness 2)
              (draw-line* stream
                          x1 y1
                          (+ x1 radius-2-dx) (+ y1 radius-2-dy)
                          :ink *selection-color*
                          :line-thickness 3)
              (draw-circle* stream
                            (+ x1 radius-2-dx) (+ y1 radius-2-dy)
                            5
                            :ink *selection-color*
                            :line-thickness 2))))))))

(defmethod move-update ((ellipse-center-handle-point ellipse-center-handle-point) dx dy)
  (with-accessors ((ellipse paint-object))
      ellipse-center-handle-point
    (with-accessors ((ink ink)
                     (filled filledp))
        ellipse
      (with-accessors ((center-point center-point)
                       (radius-1-dx radius-1-dx)
                       (radius-1-dy radius-1-dy)
                       (radius-2-dx radius-2-dx)
                       (radius-2-dy radius-2-dy)
                       (start-angle start-angle)
                       (end-angle end-angle)
                       (filledp filledp)
                       (ink ink)
                       (line-thickness line-thickness))
          ellipse
        (multiple-value-bind (x1 y1)
            (point-position center-point)
          (setf center-point (make-point (+ x1 dx)
                                         (+ y1 dy))
                radius-1-dx (- radius-1-dx dx)
                radius-1-dy (- radius-1-dy dy) 
                radius-2-dx (- radius-2-dx dx)
                radius-2-dy (- radius-2-dy dy)))))))

;;; ellipse-radius handle
(defmethod move-dragging ((ellipse-radius-handle-point ellipse-radius-handle-point) stream dx dy)
  (with-accessors ((ellipse paint-object)
                   (radius-index radius-index))
      ellipse-radius-handle-point
    (with-accessors ((ink ink)
                     (filled filledp))
        ellipse
      (with-output-to-output-record (stream)
        ;; draw the ellipse in its new form
        (with-accessors ((center-point center-point)
                         (radius-1-dx radius-1-dx)
                         (radius-1-dy radius-1-dy)
                         (radius-2-dx radius-2-dx)
                         (radius-2-dy radius-2-dy)
                         (start-angle start-angle)
                         (end-angle end-angle)
                         (filledp filledp)
                         (ink ink)
                         (line-thickness line-thickness))
            ellipse
          (multiple-value-bind (x1 y1)
              (point-position center-point)
            (let ((drag-radius-1-dx (if (= radius-index 1)
                                        (+ radius-1-dx dx)
                                        radius-1-dx))
                  (drag-radius-1-dy (if (= radius-index 1)
                                        (+ radius-1-dy dy)
                                        radius-1-dy))
                  (drag-radius-2-dx (if (= radius-index 2)
                                        (+ radius-2-dx dx)
                                        radius-2-dx))
                  (drag-radius-2-dy (if (= radius-index 2)
                                        (+ radius-2-dy dy)
                                        radius-2-dy)))
              (draw-ellipse* stream
                             x1 y1
                             drag-radius-1-dx drag-radius-1-dy
                             drag-radius-2-dx drag-radius-2-dy
                             :start-angle start-angle
                             :end-angle end-angle
                             :ink ink
                             :filled filled
                             :line-thickness line-thickness)
              (draw-circle* stream
                            x1 y1
                            5
                            :ink *selection-color*
                            :line-thickness 2)
              (draw-line* stream
                          x1 y1
                          (+ x1 drag-radius-1-dx) (+ y1 drag-radius-1-dy)
                          :ink *selection-color*
                          :line-thickness 3)
              (draw-circle* stream
                            (+ x1 drag-radius-1-dx) (+ y1 drag-radius-1-dy)
                            5
                            :ink *selection-color*
                            :line-thickness 2)
              (draw-line* stream
                          x1 y1
                          (+ x1 drag-radius-2-dx) (+ y1 drag-radius-2-dy)
                          :ink *selection-color*
                          :line-thickness 3)
              (draw-circle* stream
                            (+ x1 drag-radius-2-dx) (+ y1 drag-radius-2-dy)
                            5
                            :ink *selection-color*
                            :line-thickness 2))))))))

(defmethod move-update ((ellipse-radius-handle-point ellipse-radius-handle-point) dx dy)
  (with-accessors ((ellipse paint-object)
                   (radius-index radius-index))
      ellipse-radius-handle-point
    (with-accessors ((ink ink)
                     (filled filledp))
        ellipse
      ;; set the new parameters of the ellipse
      (if (= radius-index 1)
          (with-accessors ((radius-1-dx radius-1-dx)
                           (radius-1-dy radius-1-dy))
              ellipse
            (incf radius-1-dx dx)
            (incf radius-1-dy dy))
          (with-accessors ((radius-2-dx radius-2-dx)
                           (radius-2-dy radius-2-dy))
              ellipse
            (incf radius-2-dx dx)
            (incf radius-2-dy dy))))))


;;; 1. com-add-ellipse
(define-clim-paint-command (com-add-ellipse :name t)
    (&key
     (ink color))
  (with-accessors ((shapes shapes)
                   (default-ink ink))
      *application-frame*
    (let ((ellipse (make-paint-ellipse (make-paint-point 100 100) 50 0 0 20
                                    :ink (or ink default-ink))))
      (push ellipse shapes))))

(defmethod setup-properties-pane ((object paint-ellipse) frame)
  (let ((panes (climi::frame-panes-for-layout frame))
        (app-pane (find-pane-named frame 'app)))
    (let ((properties-pane (cdr (find 'properties panes :key #'car)))
          (center-x (cdr (find 'ellipse-center-x panes :key #'car)))
          (center-y (cdr (find 'ellipse-center-y panes :key #'car)))
          (radius-1-dx (cdr (find 'ellipse-radius-1-dx panes :key #'car)))
          (radius-1-dy (cdr (find 'ellipse-radius-1-dy panes :key #'car)))
          (radius-2-dx (cdr (find 'ellipse-radius-2-dx panes :key #'car)))
          (radius-2-dy (cdr (find 'ellipse-radius-2-dy panes :key #'car)))
          (start-angle (cdr (find 'ellipse-start-angle panes :key #'car)))
          (end-angle (cdr (find 'ellipse-end-angle panes :key #'car))))
      (multiple-value-bind (x y)
          (point-position (center-point object))
        (setf (gadget-value center-x) (princ-to-string x)
              (gadget-value center-y) (princ-to-string y)))
      (setf (gadget-value radius-1-dx) (princ-to-string (radius-1-dx object))
            (gadget-value radius-1-dy) (princ-to-string (radius-1-dy object))
            (gadget-value radius-2-dx) (princ-to-string (radius-2-dx object))
            (gadget-value radius-2-dy) (princ-to-string (radius-2-dy object)))
      (setf (gadget-value start-angle) (princ-to-string (start-angle object))
            (gadget-value end-angle) (princ-to-string (end-angle object)))
      (setf (pane-object properties-pane) object)
      (setf (pane-needs-redisplay app-pane) t)
      (setf (frame-current-layout frame) 'ellipse)
      (clim:redisplay-frame-pane frame app-pane))))

;;;
(defun ellipse-update-callback (button)
  (declare (ignore button))
  (let ((properties-pane (find-pane-named *application-frame* 'properties)))
    (let ((object (pane-object properties-pane)))
      (let ((x (parse-number:parse-number
                (gadget-value (find-pane-named *application-frame* 'ellipse-center-x))))
            (y (parse-number:parse-number
                (gadget-value (find-pane-named *application-frame* 'ellipse-center-y))))
            (radius-1-dx (parse-number:parse-number
                          (gadget-value (find-pane-named *application-frame* 'ellipse-radius-1-dx))))
            (radius-1-dy (parse-number:parse-number
                          (gadget-value (find-pane-named *application-frame* 'ellipse-radius-1-dy))))
            (radius-2-dx (parse-number:parse-number
                          (gadget-value (find-pane-named *application-frame* 'ellipse-radius-2-dx))))
            (radius-2-dy (parse-number:parse-number
                          (gadget-value (find-pane-named *application-frame* 'ellipse-radius-2-dy))))
            (start-angle (parse-number:parse-number
                          (gadget-value (find-pane-named *application-frame* 'ellipse-start-angle))))
            (end-angle (parse-number:parse-number
                        (gadget-value (find-pane-named *application-frame* 'ellipse-end-angle)))))
        (setf (center-point object) (make-point x y)
              (radius-1-dx object) radius-1-dx
              (radius-1-dy object) radius-1-dy
              (radius-2-dx object) radius-2-dx
              (radius-2-dy object) radius-2-dy
              (start-angle object) start-angle
              (end-angle object) end-angle))))
  (let* ((frame *application-frame*)
         (app-pane (find-pane-named frame 'app)))
    (setf (pane-needs-redisplay app-pane) t)
    (clim:redisplay-frame-pane *application-frame* app-pane)))

;;;
#+nil
(defmethod display-properties ((object ellipse) pane)
  (let (x-pos y-pos)
    (multiple-value-bind (x y)
        (point-position (center-point object))
      (stream-set-cursor-position pane 10 10)
      (stream-write-string pane "X pos:  ")
      (surrounding-output-with-border (pane)
        (setf x-pos
              (with-output-as-gadget (pane)
                (make-pane 'text-field :width 60 :value (princ-to-string x)))))

      #+nil
      (progn
        (stream-set-cursor-position pane 10 50)
        (stream-write-string pane "Y pos:  ")
        (surrounding-output-with-border (pane)
          (setf y-pos
                (with-output-as-gadget (pane)
                  (make-pane 'text-field :width 60 :value (princ-to-string y)))))
        (setf (pane-needs-redisplay y-pos) t)))
    (stream-set-cursor-position pane 50 250)
    (with-output-as-gadget (pane)
      (make-pane 'push-button
                 :label "Update"
                 :activate-callback (lambda (button)
                                      (declare (ignore button))
                                      (let ((x (parse-number:parse-number (gadget-value x-pos)))
                                            (y 200
                                             #+nil (parse-number:parse-number (gadget-value y-pos))))
                                        (setf (center-point object)
                                              (make-point x y)))
                                      (let* ((frame *application-frame*)
                                             (app-pane (find-pane-named frame 'app)))
                                        (setf (pane-needs-redisplay app-pane) t)
                                        (clim:redisplay-frame-pane *application-frame* app-pane)))))))
