
(in-package :clim-paint)

;;; selection handle
(defclass selection-handle-object ()
  ((paint-object :initarg :paint-object :accessor paint-object)
   (ink :initarg :ink :accessor ink)
   (line-thickness :initarg :line-thickness :accessor line-thickness)
   (filled :initarg :filled :accessor filledp)))

(defgeneric selection-handle-object-p (object)
  (:method ((object t)) nil)
  (:method ((object selection-handle-object)) t)
  (:documentation "Checking for class selection-handle-object"))

(defclass selection-handle-point (selection-handle-object)
  ((point :type point :initarg :point :accessor %point)
   (radius :initarg :radius :accessor radius)))

;;;
;;; selection-handle-point-presentation
(defclass selection-handle-point-presentation (standard-presentation) ())

(define-presentation-type selection-handle-point-presentation ())

(define-presentation-method present (selection-handle-object
                                     (type selection-handle-point) pane
                                     (view clim-paint-view)
                                     &key)
  (multiple-value-bind (x y)
      (point-position (%point selection-handle-object))
    (draw-circle* pane x y (radius selection-handle-object)
                  :ink (ink selection-handle-object)
                  :filled (filledp selection-handle-object)
                  :line-thickness 2)))

;;; selection

(define-clim-paint-command (com-select-object)
    ((presentation t))
  (let ((pane (get-frame-pane *application-frame* 'app)))
    (funcall-presentation-generic-function select-presentation
                                           (type-of (presentation-object presentation))
                                           presentation pane :select)))

(define-gesture-name select-gesture :pointer-button (:middle))

(define-presentation-to-command-translator select-paint-object-translator
    (paint-object com-select-object clim-paint
                  :gesture select-gesture
                  :menu nil
                  :tester ((object)
                           t))
    (object presentation)
  (list presentation))

(define-presentation-to-command-translator select-selection-handle-object-translator
    (selection-handle-object com-select-object clim-paint
                  :gesture select-gesture
                  :menu nil
                  :tester ((object)
                           t))
    (object presentation)
  (list presentation))

(define-presentation-method select-presentation
    ((type paint-object) (record presentation) stream state)
  (let* ((frame *application-frame*))
    (case state
      (:select
       (clrhash (selected-object-hash frame))
       (let ((object (presentation-object record)))
         (setf (gethash object (selected-object-hash frame)) t)
         (typecase object

           (paint-point
            (let ((panes (climi::frame-panes-for-layout frame)))
              (let ((properties-pane (cdr (find 'properties panes :key #'car)))
                    (x-pos (cdr (find 'point-x-pos panes :key #'car)))
                    (y-pos (cdr (find 'point-y-pos panes :key #'car))))
                (multiple-value-bind (x1 y1)
                    (point-position object)
                  (setf (gadget-value x-pos) (princ-to-string x1)
                        (gadget-value y-pos) (princ-to-string y1)))
                (setf (pane-object properties-pane) object)
                (setf (frame-current-layout frame) 'point)
                (setf (pane-needs-redisplay properties-pane) t)
                (clim:redisplay-frame-pane frame properties-pane))))

           (paint-line
            (let ((panes (climi::frame-panes-for-layout frame)))
              (let ((properties-pane (cdr (find 'properties panes :key #'car)))
                    (x1-pos (cdr (find 'line-x1-pos panes :key #'car)))
                    (y1-pos (cdr (find 'line-y1-pos panes :key #'car)))
                    (x2-pos (cdr (find 'line-x2-pos panes :key #'car)))
                    (y2-pos (cdr (find 'line-y2-pos panes :key #'car))))
                (multiple-value-bind (x1 y1)
                    (point-position (line-start-point* object))
                  (multiple-value-bind (x2 y2)
                      (point-position (line-end-point* object))
                    (setf (gadget-value x1-pos) (princ-to-string x1)
                          (gadget-value y1-pos) (princ-to-string y1)
                          (gadget-value x2-pos) (princ-to-string x2)
                          (gadget-value y2-pos) (princ-to-string y2))))
                (setf (pane-object properties-pane) object)
                (setf (frame-current-layout frame) 'line)
                (setf (pane-needs-redisplay properties-pane) t)
                (clim:redisplay-frame-pane frame properties-pane))))

           (paint-rectangle
            (let ((panes (climi::frame-panes-for-layout frame)))
              (let ((properties-pane (cdr (find 'properties panes :key #'car)))
                    (x1-pos (cdr (find 'rectangle-x1-pos panes :key #'car)))
                    (y1-pos (cdr (find 'rectangle-y1-pos panes :key #'car)))
                    (x2-pos (cdr (find 'rectangle-x2-pos panes :key #'car)))
                    (y2-pos (cdr (find 'rectangle-y2-pos panes :key #'car))))
                (multiple-value-bind (x1 y1)
                    (point-position (%point-1 object))
                  (multiple-value-bind (x2 y2)
                      (point-position (%point-2 object))
                    (setf (gadget-value x1-pos) (princ-to-string x1)
                          (gadget-value y1-pos) (princ-to-string y1)
                          (gadget-value x2-pos) (princ-to-string x2)
                          (gadget-value y2-pos) (princ-to-string y2))))
                (setf (pane-object properties-pane) object)
                (setf (frame-current-layout frame) 'rectangle)
                (setf (pane-needs-redisplay properties-pane) t)
                (clim:redisplay-frame-pane frame properties-pane))))

           (paint-ellipse
            (let ((panes (climi::frame-panes-for-layout frame)))
              (let ((properties-pane (cdr (find 'properties panes :key #'car)))
                    (x-pos (cdr (find 'ellipse-x-pos panes :key #'car)))
                    (y-pos (cdr (find 'ellipse-y-pos panes :key #'car))))
                (multiple-value-bind (x y)
                    (point-position (center-point object))
                  (setf (gadget-value x-pos) (princ-to-string x)
                        (gadget-value y-pos) (princ-to-string y)))
                (setf (pane-object properties-pane) object)
                (setf (frame-current-layout frame) 'ellipse)
                (setf (pane-needs-redisplay properties-pane) t)
                (clim:redisplay-frame-pane frame properties-pane))))

           (paint-bezier-curve
            (let ((panes (climi::frame-panes-for-layout frame)))
              (let ((properties-pane (cdr (find 'properties panes :key #'car))))

                ;;; FIXME!!

                (setf (pane-object properties-pane) object)
                (setf (frame-current-layout frame) 'bezier-curve)
                (setf (pane-needs-redisplay properties-pane) t)
                (clim:redisplay-frame-pane frame properties-pane))))))

       (queue-repaint stream
                      (make-instance 'window-repaint-event
                                     :sheet stream
                                     :region +everywhere+)))
      (:deselect
       (clrhash (selected-object-hash frame))
       (let ((panes (climi::frame-panes-for-layout frame)))
         (let ((properties-pane (cdr (find 'properties panes :key #'car))))
           (setf (pane-object properties-pane) nil)))
       (setf (frame-current-layout frame) 'default)
       (queue-repaint
        stream
        (make-instance 'window-repaint-event
                       :sheet stream
                       :region +everywhere+))))
    #+nil
    ;; or should we do it like this?
    (queue-repaint properties-pane
                   (make-instance 'window-repaint-event
                                  :sheet properties-pane
                                  :region +everywhere+))))

