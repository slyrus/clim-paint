
(in-package :clim-paint)

;;;
;;; some special variables to be used for drawing/dragging
(defparameter *background-color* +black+)

(defparameter *foreground-color* +white+)

(defparameter *highlight-color* +orange+)

(defparameter *selection-color* +red+)

(defparameter *drag-color* +green+)

(defgeneric move-dragging (paint-object stream dx dy))

(defgeneric move-update (paint-object dx dy))

;;; I think this is currently unused.
(defun find-top-level-output-record (record)
  (when record
    (with-accessors ((parent output-record-parent))
        record
      (if (null parent)
          record
          (find-top-level-output-record parent)))))


(defun get-pointer-position (pane)
  "Returns a point with x and y values of the stream-pointer-position
of pane."
  (multiple-value-bind (x y) (stream-pointer-position pane)
    (make-point x y)))

;; selection -- still a WIP
(define-presentation-generic-function %select-presentation
    select-presentation
  (climi::type-key climi::parameters climi::options climi::type record stream state))

(define-default-presentation-method select-presentation
    (type record stream state)
  (declare (ignore type record stream state)))

(defclass clim-paint-presentation (standard-presentation) ())

(defparameter *clim-paint-presentation-print-float-digits* 2)

(defmethod print-object ((object clim-paint-presentation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-bounding-rectangle* (x1 y1 x2 y2)
        object
      (format stream
              (format nil "[~~,~D~:*F:~~,~D~:*F] [~~,~D~:*F:~~,~D~:*F] ~~S"
                      *clim-paint-presentation-print-float-digits*)
              x1 x2 y1 y2 (presentation-type object))
      (when climi::*print-presentation-verbose*
        (format stream " ~S" (presentation-object object))))))

