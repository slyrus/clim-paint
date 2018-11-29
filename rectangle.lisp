
(in-package :clim-paint)

;;;
;;; rectangle
(defclass paint-rectangle (paint-object)
  ((point-1 :type point :initarg :point-1 :accessor %point-1)
   (point-2 :type point :initarg :point-2 :accessor %point-2)))

(defgeneric paint-rectangle-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-rectangle)) t)
  (:documentation "Checking for class paint-rectangle"))

(defun make-paint-rectangle (x1 y1 x2 y2 &key (ink nil ink-supplied-p)
                                              (filled nil filled-supplied-p))
  (apply #'make-instance 'paint-rectangle
         :point-1 (make-point (coerce x1 'coordinate)
                              (coerce y1 'coordinate))
         :point-2 (make-point (coerce x2 'coordinate)
                              (coerce y2 'coordinate))
         (append
          (when ink-supplied-p
            `(:ink ,ink))
          (when filled-supplied-p
            `(:filled ,filled)))))

(defmethod bounding-rectangle* ((object paint-rectangle))
  (multiple-value-bind (x1 y1)
      (point-position (%point-1 object))
    (multiple-value-bind (x2 y2)
        (point-position (%point-2 object))
      (values x1 y1 x2 y2))))

;;;
;;; rectangle-presentation
(defclass rectangle-presentation (standard-presentation) ())

(define-presentation-type rectangle-presentation ())

;;
;; subclass selection-handle-point which has a point, which is the
;; point of the selection-handle itself, but only a single reference
;; to the selected object. So use this class to add a slot for the
;; particular point on the rectangle which was selected. Previously I
;; had just stored a cons in the paint-object slot, but it seems
;; better to be explicit about what information we want to store and
;; to not break the semantics of the selection-handle-object
;; superclass should any methods expect paint-object to just be an
;; object, not a cons.
(defclass rectangle-handle-point (selection-handle-point)
  ((rectangle-point :initarg :rectangle-point :accessor rectangle-point)))

(defparameter *rectangle-selection-width* 4)

(defun draw-rectangle-selection (pane rectangle &key (ink +black+)
                                                     (radius 8)
                                                     (filled nil))
  (multiple-value-bind (x1 y1)
      (point-position (%point-1 rectangle))
    (multiple-value-bind (x2 y2)
        (point-position (%point-2 rectangle))
      (let ((sx1 (if (>= x2 x1)
                     (- x1 *rectangle-selection-width*)
                     (+ x1 *rectangle-selection-width* -1)))
            (sy1 (if (>= y2 y1)
                     (- y1 *rectangle-selection-width*)
                     (+ y1 *rectangle-selection-width* -1)))
            (sx2 (if (>= x2 x1)
                     (+ x2 *rectangle-selection-width* -1)
                     (- x2 *rectangle-selection-width*)))
            (sy2 (if (>= y2 y1)
                     (+ y2 *rectangle-selection-width* -1)
                     (- y2 *rectangle-selection-width*))))
        (draw-rectangle* pane sx1 sy1 sx2 sy2
                         :ink *selection-color*
                         :filled nil
                         :line-dashes t)
        (present (make-instance 'rectangle-handle-point
                                :paint-object rectangle
                                :rectangle-point (%point-1 rectangle)
                                :point (make-point sx1 sy1)
                                :ink ink
                                :radius radius
                                :filled filled)
                 'rectangle-handle-point
                 :record-type 'selection-handle-point-presentation
                 :single-box t)
        (present (make-instance 'rectangle-handle-point
                                :paint-object rectangle
                                :rectangle-point (%point-2 rectangle)
                                :point (make-point sx2 sy2)
                                :ink ink
                                :radius radius
                                :filled filled)
                 'rectangle-handle-point
                 :record-type 'selection-handle-point-presentation
                 :single-box t)))))

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
                       :filled filled)))
  (if (gethash rectangle *selected-object-hash*)
        (draw-rectangle-selection pane rectangle)))

;;;
;;; highlighting
(defparameter *rectangle-highlight-margin* 2)

(define-presentation-method highlight-presentation
    ((type paint-rectangle) (record rectangle-presentation) stream state)
  (let ((rectangle (presentation-object record)))
    (case state
      (:highlight
       (multiple-value-bind (x1 y1)
           (point-position (%point-1 rectangle))
         (multiple-value-bind (x2 y2)
             (point-position (%point-2 rectangle))
           (draw-rectangle* stream
                            (- x1 *rectangle-highlight-margin* 1)
                            (- y1 *rectangle-highlight-margin* 1)
                            (+ x2 *rectangle-highlight-margin*)
                            (+ y2 *rectangle-highlight-margin*)
                            :ink *highlight-color*
                            :filled nil
                            :line-thickness *rectangle-highlight-margin*))))
      (:unhighlight
       (queue-repaint
        stream
        (make-instance
         'window-repaint-event
         :sheet stream
         :region (transform-region
                  (sheet-native-transformation stream)
                  (multiple-value-bind (x1 y1)
                      (point-position (%point-1 rectangle))
                    (multiple-value-bind (x2 y2)
                        (point-position (%point-2 rectangle))
                      (make-rectangle* (- x1 *rectangle-highlight-margin* 2)
                                       (- y1 *rectangle-highlight-margin* 2)
                                       (+ x2 *rectangle-highlight-margin* 2)
                                       (+ y2 *rectangle-highlight-margin* 2)))))))))))

;;;
;;; dragging / moving
(defmethod move-dragging ((rectangle paint-rectangle) stream dx dy)
  (with-output-to-output-record (stream)
    (multiple-value-bind (x1 y1)
        (point-position (%point-1 rectangle))
      (multiple-value-bind (x2 y2)
          (point-position (%point-2 rectangle))
        (draw-rectangle* stream (+ x1 dx) (+ y1 dy) (+ x2 dx) (+ y2 dy)
                         :ink (ink rectangle)
                         :filled (filledp rectangle))))))

(defmethod move-update ((rectangle paint-rectangle) dx dy)
  (with-accessors ((point-1 %point-1)
                   (point-2 %point-2))
      rectangle
    (multiple-value-bind (x1 y1)
        (point-position point-1)
      (multiple-value-bind (x2 y2)
          (point-position point-2)
        (setf point-1 (make-point (+ x1 dx) (+ y1 dy))
              point-2 (make-point (+ x2 dx) (+ y2 dy)))))))


;;; 3. selection handle dragging / moving
(defun rectangle-other-point (rectangle point)
  (if (eql (%point-1 rectangle) point)
      (%point-2 rectangle)
      (%point-1 rectangle)))

(defmethod move-dragging ((rectangle-handle-point rectangle-handle-point) stream dx dy)
  (with-output-to-output-record (stream)
    (with-accessors ((rectangle paint-object)
                     (my-point rectangle-point))
        rectangle-handle-point
      (with-accessors ((ink ink)
                       (filled filledp))
          rectangle
        (multiple-value-bind (near-x near-y)
            (point-position my-point)
          (multiple-value-bind (other-x other-y)
              (point-position (rectangle-other-point rectangle my-point))
            (draw-rectangle* stream
                             (+ near-x dx) (+ near-y dy)
                             other-x other-y
                             :ink ink :filled filled))))
      (multiple-value-bind (x1 y1)
          (point-position (%point rectangle-handle-point))
        (draw-circle* stream (+ x1 dx) (+ y1 dy)
                      (radius rectangle-handle-point)
                      :ink (ink rectangle-handle-point)
                      :filled (filledp rectangle-handle-point)
                      :line-thickness 2)))))

(defmethod move-update ((rectangle-handle-point rectangle-handle-point) dx dy)
  (with-accessors ((rectangle paint-object)
                   (my-point rectangle-point))
      rectangle-handle-point
    (multiple-value-bind (near-x near-y)
        (point-position my-point)
      (multiple-value-bind (other-x other-y)
          (point-position (rectangle-other-point rectangle my-point))
        (setf (%point-1 rectangle) (make-point
                                    (+ near-x dx)
                                    (+ near-y dy))
              (%point-2 rectangle) (make-point
                                    other-x
                                    other-y))))))

