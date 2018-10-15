
(in-package :clim-paint)

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
;;; rectangle-presentation
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

