
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

;;
;; dragging-output doesn't support the feedback arg. No reason it
;; shouldn't so let's take McCLIM's version and make a new version
;; that takes a feedback arg.
(locally
    (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (defun invoke-with-dragging-output* (continuation
                                       &optional (stream *standard-output*)
                                       &rest args
                                       &key repaint
                                            finish-on-release
                                            multiple-window)
    (declare #+sbcl (sb-ext:unmuffle-conditions style-warning)
             (ignore repaint finish-on-release multiple-window))
    (let ((record (with-output-to-output-record (stream))))
      (flet ((feedback-fn (record stream initial-x initial-y x y event)
               (multiple-value-bind (record-x record-y)
                   (output-record-position record)
                 (let ((dx (- record-x initial-x))
                       (dy (- record-y initial-y)))
                   (case event
                     (:draw
                      (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2)
                          record
                        (when (output-record-parent record)
                          (delete-output-record record (output-record-parent record)))
                        (setf (output-record-position record)
                              (values (+ dx x) (+ dy y)))
                        (add-output-record (funcall continuation stream x y) record)
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
                          (stream-replay stream buffer-rectangle)))))))))
        (apply #'drag-output-record stream record :erase-final t :feedback #'feedback-fn args)))))

(locally
    (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (defmacro dragging-output* ((&optional (stream *standard-output*) &rest args
                                         &key (repaint t)
                                         finish-on-release
                                         multiple-window)
                               &body body)
    "An enhanced version of the clim:dragging-output macro. The
original macro doesn't take a feedback function, but this one does and
the feeedback function is then passed to drag-output-record. The
feedback function is provided as the body in calls to this macro and
must consist of a function that takes three arguments, the stream, and
teh x and y position of the pointer, and returns an output record of
the drawn feedback. This output record is inserted into the stream's
current output-record upon drawing, and is subsequently removed upon
erasing."
    (declare #+sbcl (sb-ext:unmuffle-conditions style-warning)
             (ignore repaint finish-on-release multiple-window))
    `(invoke-with-dragging-output* ,@body ,stream ,@args)))


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

