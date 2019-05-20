
(in-package :clim-paint)

;;;
;;; paint lines
(defclass paint-line (paint-object)
  ((p1 :type paint-point :initarg :p1)
   (p2 :type paint-point :initarg :p2)))

(defgeneric paint-line-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-line)) t)
  (:documentation "Checking for class paint-line"))

(defun make-paint-line (start-point end-point &key ink)
  (apply #'make-instance 'paint-line :p1 start-point :p2 end-point
         (when ink `(:ink ,ink))))

(defmethod line-start-point* ((line paint-line))
  (with-slots (p1) line
    (point-position p1)))

(defmethod line-end-point* ((line paint-line))
  (with-slots (p2) line
    (point-position p2)))

(defmethod line-start-point ((line paint-line))
  (with-slots (p1) line
    p1))

(defmethod line-end-point ((line paint-line))
  (with-slots (p2) line
    p2))

(defun find-lines-containing (point shapes)
  (loop for shape in shapes
     when (and (paint-line-p shape)
               (or (eq point (line-start-point shape))
                   (eq point (line-end-point shape))))
     collect shape))

(defparameter *line-selection-width* 8)

(defun draw-line-selection (pane line)
  (with-accessors ((ink ink)
                   (start line-start-point)
                   (end line-end-point))
      line
    ;; draw a rectangle around line, say, 3 pixels wider on either side
    (multiple-value-bind  (x1 y1)
        (point-position start)
      (multiple-value-bind  (x2 y2)
          (point-position end)
        (let ((theta (phase (complex (- y2 y1) (- x2 x1)))))
          (let ((theta1a (- theta (* pi (/ 1 4))))
                (theta1b (+ theta (* pi (/ 1 4)))))
            (draw-polygon* pane
                           (list (- x1 (* *line-selection-width* (sin theta1a)))
                                 (- y1 (* *line-selection-width* (cos theta1a)))
                                 (- x1 (* *line-selection-width* (sin theta1b)))
                                 (- y1 (* *line-selection-width* (cos theta1b)))
                                 (+ x2 (* *line-selection-width* (sin theta1a)))
                                 (+ y2 (* *line-selection-width* (cos theta1a)))
                                 (+ x2 (* *line-selection-width* (sin theta1b)))
                                 (+ y2 (* *line-selection-width* (cos theta1b))))
                           :ink *selection-color*
                           :line-thickness 2
                           :line-dashes t
                           :filled nil)))))))

(define-presentation-method present (line (type paint-line) pane
                                          (view clim-paint-view) &key)
  (with-accessors ((ink ink)
                   (start line-start-point)
                   (end line-end-point))
      line
    (draw-line pane start end :ink ink)
    (if (gethash line (selected-object-hash *application-frame*))
        (draw-line-selection pane line))))

;;;
;;; refined-position test
(define-presentation-method presentation-refined-position-test
    ((type paint-line) record x y)
  (let ((line (presentation-object record)))
    (line-point-between-p (make-point x y)
                          (line-start-point line)
                          (line-end-point line))))

;;;
;;; highlighting
(define-presentation-method highlight-presentation
    ((type paint-line) record stream state)
  (let ((line (presentation-object record)))
    (with-accessors ((start line-start-point)
                     (end line-end-point))
        line
      (case state
        (:highlight
         (draw-line stream start end
                    :line-thickness 4 :ink *highlight-color*))
        (:unhighlight
         (queue-repaint
          stream
          (make-instance 'window-repaint-event
                         :sheet stream
                         :region (transform-region
                                  (sheet-native-transformation stream)
                                  record))))))))

;;; commands

;;; 1. com-add-line
(define-clim-paint-command (com-add-line :name t)
    ((point1 point :prompt "Point 1")
     (point2 point :prompt "Point 2")
     &key
     (ink color))
  (with-accessors ((shapes shapes)
                   (default-ink ink))
      *application-frame*
    (when (and point1 point2))
    (let ((line (make-paint-line point1
                                 point2
                                 :ink (or ink default-ink))))
      (push line shapes))))

;;; 2. com-drag-split-line
(define-clim-paint-command (com-drag-split-line)
    ((line paint-line) (presentation presentation) (frame frame))
  (declare (ignore presentation))
  (with-accessors ((ink ink)
                   (shapes shapes))
      frame
    (let ((pane (get-frame-pane frame 'app))
          (new-ink (or (ink line) ink)))
      (multiple-value-bind (x y)
          (dragging-output (pane :finish-on-release t)
            (draw-circle pane (get-pointer-position pane) 6
                         :ink new-ink :filled t))
        (let ((p1 (find (line-start-point line) shapes :test 'paint-point=))
              (p2 (find (line-end-point line) shapes :test 'paint-point=)))
          (setf shapes (delete-if (lambda (x)
                                    (and (paint-line-p x)
                                         (or
                                          (and (paint-point= (line-start-point x) p1)
                                               (paint-point= (line-end-point x) p2))
                                          (and (paint-point= (line-start-point x) p2)
                                               (paint-point= (line-end-point x) p1)))))
                                  shapes))
          (let ((new-point (com-add-point x y :ink new-ink)))
            (com-add-line p1 new-point :ink new-ink)
            (com-add-line p2 new-point :ink new-ink))))
      (setf (pane-needs-redisplay pane) t))))

;;; 3. com-split-line
(define-clim-paint-command (com-split-line)
    ((presentation t))
  (let ((line (presentation-object presentation)))
    (com-drag-split-line line presentation *application-frame*)))

(define-gesture-name split-line-gesture :pointer-button (:right))

(define-presentation-to-command-translator split-line-translator
    (paint-line com-split-line clim-paint
          :gesture split-line-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))



;;;
;;; dragging / moving
(defmethod move-dragging ((line paint-line) stream dx dy)
  (with-accessors ((shapes shapes))
      *application-frame*
    (flet ((connect-neighbors (paint-point)
             (let ((neighbors
                    (remove line (find-lines-containing paint-point shapes))))
               (loop for other-line in neighbors
                  do (let ((other-paint-point
                            (if (eq (line-start-point other-line) paint-point)
                                (line-end-point other-line)
                                (line-start-point other-line))))
                       (multiple-value-bind (nx1 ny1)
                           (point-position paint-point)
                         (multiple-value-bind (nx2 ny2)
                             (point-position other-paint-point)
                           (draw-line* stream (+ nx1 dx) (+ ny1 dy) nx2 ny2
                                       :line-thickness 4
                                       :ink *drag-color*))))))))
      (with-output-to-output-record (stream)
        (let ((paint-point-1 (line-start-point line))
              (paint-point-2 (line-end-point line)))
          (multiple-value-bind (x1 y1)
              (point-position paint-point-1)
            (multiple-value-bind (x2 y2)
                (point-position paint-point-2)
              (with-accessors ((ink ink))
                  line
                (draw-circle* stream (+ x1 dx) (+ y1 dy) 6
                              :ink ink :filled t)
                (draw-circle* stream (+ x2 dx) (+ y2 dy) 6
                              :ink ink :filled t)
                (connect-neighbors paint-point-1)
                (connect-neighbors paint-point-2)
                (draw-line* stream (+ x1 dx) (+ y1 dy) (+ x2 dx) (+ y2 dy)
                            :line-thickness 4
                            :ink *drag-color*)))))))))

(defmethod move-update ((line paint-line) dx dy)
  (let ((paint-point-1 (line-start-point line))
        (paint-point-2 (line-end-point line)))
    (multiple-value-bind (x1 y1)
        (point-position paint-point-1)
      (setf (%point paint-point-1)
            (make-point (+ x1 dx) (+ y1 dy))))
    (multiple-value-bind (x2 y2)
        (point-position paint-point-2)
      (setf (%point paint-point-2)
            (make-point (+ x2 dx) (+ y2 dy))))))


;;;
;;; line-update-callback
(defun line-update-callback (button)
  (declare (ignore button))
  (let ((properties-pane (find-pane-named *application-frame* 'properties)))
    (let ((object (pane-object properties-pane)))
      (let ((x1 (parse-number:parse-number
                 (gadget-value (find-pane 'x1-pos properties-pane))))
            (y1 (parse-number:parse-number
                 (gadget-value (find-pane 'y1-pos properties-pane))))
            (x2 (parse-number:parse-number
                 (gadget-value (find-pane 'x2-pos properties-pane))))
            (y2 (parse-number:parse-number
                 (gadget-value (find-pane 'y2-pos properties-pane)))))
        (let ((paint-point-1 (line-start-point object))
              (paint-point-2 (line-end-point object)))
          (setf (%point paint-point-1) (make-point x1 y1)
                (%point paint-point-2) (make-point x2 y2))))))
  (let* ((frame *application-frame*)
         (app-pane (find-pane-named frame 'app)))
    (setf (pane-needs-redisplay app-pane) t)
    (clim:redisplay-frame-pane *application-frame* app-pane)))

(defmethod make-properties-pane ((line paint-line))
  (multiple-value-bind (x1 y1)
      (line-start-point* line)
    (multiple-value-bind (x2 y2)
        (line-end-point* line)
      (make-pane
       'properties-pane
       :name 'properties
       :contents
       (list
        (labelling (:label "Line Properties")
          (vertically ()
            (horizontally ()
              (labelling (:label "X1 Position")
                (make-pane 'text-field :name 'x1-pos :editable-p t :value (princ-to-string x1)))
              (labelling (:label "Y1 Position")
                (make-pane 'text-field :name 'y1-pos :editable-p t :value (princ-to-string y1))))
            (horizontally ()
              (labelling (:label "X2 Position")
                (make-pane 'text-field :name 'x2-pos :editable-p t :value (princ-to-string x2)))
              (labelling (:label "Y2 Position")
                (make-pane 'text-field :name 'y2-pos :editable-p t :value (princ-to-string y2))))
            (make-pane 'push-button
                       :name 'line-update
                       :label "Update"
                       :activate-callback 'line-update-callback
                       :max-height 20))))))))

