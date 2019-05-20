
(in-package :clim-paint)

;;;
;;; paint points
(defclass paint-point (paint-object)
  ((point :type point :initarg :point :accessor %point)))

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

(defun paint-point= (p1 p2)
  (when (and (paint-point-p p1) (paint-point-p p2))
    (multiple-value-bind (x1 y1)
        (point-position p1)
      (multiple-value-bind (x2 y2)
          (point-position p2)
        (and (= x1 x2)
             (= y1 y2))))))

(defparameter *point-selection-width* 10)

(defclass point-handle-point (selection-handle-point) ())

(defun draw-point-selection (pane point &key (ink *foreground-color*)
                                             (radius 10)
                                             (filled nil))
  (declare (ignore pane))
  #+nil
  (present (make-instance 'point-handle-point
                          :paint-object point
                          :point (%point point)
                          :ink ink
                          :radius radius
                          :filled filled)
           'point-handle-point
           :single-box t))

(define-presentation-method present (object (type paint-point) pane
                                            (view clim-paint-view) &key)
  (multiple-value-bind (x y)
      (point-position object)
    (with-accessors ((ink ink))
        object
      (draw-circle* pane x y 6 :ink ink :filled t)))
  (if (gethash object (selected-object-hash *application-frame*))
      (draw-point-selection pane object)))

;;;
;;; highlighting
(define-presentation-method highlight-presentation
    ((type paint-point) record stream state)
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

;;;
;;; commands


;;;
;;; dragging / moving
(defmethod move-dragging ((point paint-point) stream dx dy)
  (with-accessors ((shapes shapes))
      *application-frame*
    (flet ((connect-neighbors (point)
             (let ((neighbors
                    (find-lines-containing point shapes)))
               (loop for other-line in neighbors
                  do (let ((other-point
                            (if (eq (line-start-point other-line) point)
                                (line-end-point other-line)
                                (line-start-point other-line))))
                       (multiple-value-bind (nx1 ny1)
                           (point-position point)
                         (multiple-value-bind (nx2 ny2)
                             (point-position other-point)
                           (draw-line* stream
                                       (+ nx1 dx) (+ ny1 dy) nx2 ny2
                                       :line-thickness 4
                                       :ink *drag-color*))))))))
      (with-output-to-output-record (stream)
        (multiple-value-bind (x1 y1)
            (point-position point)
          (with-accessors ((ink ink))
              point
            (draw-circle* stream (+ x1 dx) (+ y1 dy) 6
                          :ink ink :filled t))
          (connect-neighbors point))))))

(defmethod move-update ((point paint-point) x y)
  (multiple-value-bind (x1 y1)
      (point-position point)
    (setf (%point point) (make-point (+ x1 x) (+ y1 y)))))


;;; 3. com-add-point
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

;;; 4. com-drag-add-point
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
                  (when ink `(:ink ,ink))))))
      (setf (pane-needs-redisplay pane) t))))

(define-gesture-name add-point-gesture :pointer-button (:right))

(define-presentation-to-command-translator point-dragging-add-translator
    (paint-point com-drag-add-point clim-paint
                 :gesture add-point-gesture
                 :menu nil
                 :tester ((object presentation event)
                          (declare (ignore presentation event))
                          (paint-point-p object)))
    (object)
  (list object))

(defun point-update-callback (button)
  (declare (ignore button))
  (let ((properties-pane (find-pane-named *application-frame* 'properties)))
    (let ((object (pane-object properties-pane)))
      (let ((x (parse-number:parse-number
                (gadget-value (find-pane 'x-pos properties-pane))))
            (y (parse-number:parse-number
                (gadget-value (find-pane 'y-pos properties-pane)))))
        (setf (%point object) (make-point x y)))))
  (let* ((frame *application-frame*)
         (app-pane (find-pane-named frame 'app)))
    (setf (pane-needs-redisplay app-pane) t)
    (clim:redisplay-frame-pane *application-frame* app-pane)))

(defmethod make-properties-pane ((point paint-point))
  (multiple-value-bind (x y)
      (point-position point)
    (make-pane
     'properties-pane
     :name 'properties
     :object point
     :contents
     (list
      (labelling (:label "Point Properties")
        (vertically ()
          (horizontally ()
            (labelling (:label "X Position")
              (make-pane 'text-field
                         :name 'x-pos
                         :editable-p t
                         :value (princ-to-string x)))
            (labelling (:label "Y Position")
              (make-pane 'text-field
                         :name 'y-pos
                         :editable-p t
                         :value (princ-to-string y))))
          (make-pane 'push-button
                     :name 'point-update
                     :label "Update"
                     :activate-callback 'point-update-callback
                     :max-height 20)))))))
