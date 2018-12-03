
(in-package :clim-paint)

;;
;; properties-pane
(defclass properties-pane (vbox-pane)
  ((pane-object :initform nil :accessor pane-object)))

;;
;; properties-view
(defclass properties-view (view)
  ())

(defgeneric display-properties (object pane)
  (:method (object pane)))

;;
;; properties
(defun properties-display (frame pane)
  (with-accessors ((shapes shapes))
      frame
    (when (pane-object pane)
      (draw-text* pane
                  (format nil "~A" (type-of (pane-object pane)))
                  10 300 :ink +black+)
      (display-properties (pane-object pane) pane))))

(defmethod frame-input-context-button-press-handler :before
    ((frame standard-application-frame)
     (stream properties-pane)
     button-press-event)
  (let ((previous (stream-set-input-focus stream)))
    (when (and previous (typep previous 'gadget))
      (let ((client (gadget-client previous))
            (id (gadget-id previous)))
      (disarmed-callback previous client id)))))

