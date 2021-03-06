
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
       #+nil
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
                      ;; We shouldn't need the bounding rectangle
                      ;; below, but commit "75683768 repaint-sheet:
                      ;; don't call bounding-rectangle on passed
                      ;; region" broke our unhighlighting code. The is
                      ;; discussed in McCLIM Issue 766. Leave in for
                      ;; now to avoid the problem.
                      (bounding-rectangle
                       (make-ellipse* x1 y1
                                      (+ radius-1-dx *ellipse-highlight-margin* 1)
                                      (+ radius-1-dy *ellipse-highlight-margin* 1)
                                      (+ radius-2-dx *ellipse-highlight-margin* 1)
                                      (+ radius-2-dy *ellipse-highlight-margin* 1))))))))))))

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

(defun ellipse-update-callback (button)
  (declare (ignore button))
  (let ((properties-pane (find-pane-named *application-frame* 'properties)))
    (let ((object (pane-object properties-pane)))
      (let ((x (parse-number:parse-number
                (gadget-value (find-pane 'center-x properties-pane))))
            (y (parse-number:parse-number
                (gadget-value (find-pane 'center-y properties-pane))))
            (radius-1-dx
             (parse-number:parse-number
              (gadget-value (find-pane 'radius-1-dx properties-pane))))
            (radius-1-dy
             (parse-number:parse-number
              (gadget-value (find-pane 'radius-1-dy properties-pane))))
            (radius-2-dx
             (parse-number:parse-number
              (gadget-value (find-pane 'radius-2-dx properties-pane))))
            (radius-2-dy
             (parse-number:parse-number
              (gadget-value (find-pane 'radius-2-dy properties-pane))))
            (start-angle
             (parse-number:parse-number
              (gadget-value (find-pane 'start-angle properties-pane))))
            (end-angle
             (parse-number:parse-number
              (gadget-value (find-pane 'end-angle properties-pane)))))
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

(defmethod make-properties-pane ((ellipse paint-ellipse))
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
      (make-pane
       'properties-pane
       :name 'properties
       :object ellipse
       :contents
       (list
        (labelling (:label "Ellipse Properties")
          (vertically ()
            (horizontally ()
              (labelling (:label "Center X")
                (make-pane 'text-field
                           :name 'center-x
                           :editable-p t
                           :value (princ-to-string x1)))
              (labelling (:label "Center Y")
                (make-pane 'text-field
                           :name 'center-y
                           :editable-p t
                           :value (princ-to-string y1))))
            (horizontally ()
              (labelling (:label "Radius 1 DX")
                (make-pane 'text-field
                           :name 'radius-1-dx
                           :editable-p t
                           :value (princ-to-string radius-1-dx)))
              (labelling (:label "Radius 1 DY")
                (make-pane 'text-field
                           :name 'radius-1-dy
                           :editable-p t
                           :value (princ-to-string radius-1-dy))))
            (horizontally ()
              (labelling (:label "Radius 2 DX")
                (make-pane 'text-field
                           :name 'radius-2-dx
                           :editable-p t
                           :value (princ-to-string radius-2-dx)))
              (labelling (:label "Radius 2 DY")
                (make-pane 'text-field
                           :name 'radius-2-dy
                           :editable-p t
                           :value (princ-to-string radius-2-dy))))
            (horizontally ()
              (labelling (:label "Start Angle")
                (make-pane 'text-field
                           :name 'start-angle
                           :editable-p t
                           :value (princ-to-string start-angle)))
              (labelling (:label "End Angle")
                (make-pane 'text-field
                           :name 'end-angle
                           :editable-p t
                           :value (princ-to-string end-angle))))
            (make-pane 'push-button
                       :name 'ellipse-update
                       :label "Update"
                       :activate-callback 'ellipse-update-callback
                       :max-height 20))))))))
