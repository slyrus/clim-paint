
(in-package :clim-paint)

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

