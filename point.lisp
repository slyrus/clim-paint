
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

;;;
;;; point-presentation
(defclass point-presentation (standard-presentation) ())

(define-presentation-type point-presentation ())

(defparameter *point-selection-width* 10)

(defun draw-point-selection (pane point &key (ink +black+)
                                             (radius 10)
                                             (filled nil))
  (declare (ignore pane))
  (present (make-instance 'selection-handle-point
                          :point (%point point)
                          :ink ink
                          :radius radius
                          :filled filled)
           'selection-handle-point
           :record-type 'selection-handle-point-presentation
           :single-box t))

(define-presentation-method present (object (type paint-point) pane
                                            (view clim-paint-view) &key)
  (multiple-value-bind (x y)
      (point-position object)
    (with-accessors ((ink ink))
        object
      (draw-circle* pane x y 6 :ink ink :filled t)))
  (if (gethash object *selected-object-hash*)
      (draw-point-selection pane object)))

;;;
;;; highlighting
(define-presentation-method highlight-presentation
    ((type paint-point) (record point-presentation) stream state)
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

;;; 1. com-drag-move-point
(define-clim-paint-command (com-drag-move-point)
    ((paint-point paint-point))
  (with-accessors ((shapes shapes))
      *application-frame*
    (with-accessors ((ink ink))
      paint-point
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (startx starty)
            (stream-pointer-position pane)
          (multiple-value-bind (x y)
              (dragging-output*
                  (pane :finish-on-release t)
                (lambda (stream x y)
                  (flet ((connect-neighbors (paint-point)
                           (let ((neighbors
                                  (find-lines-containing paint-point shapes)))
                             (loop for other-line in neighbors
                                do (let ((other-paint-point
                                          (if (eq (line-start-point other-line) paint-point)
                                              (line-end-point other-line)
                                              (line-start-point other-line))))
                                     (multiple-value-bind (nx1 ny1)
                                         (point-position paint-point)
                                       (multiple-value-bind (nx2 ny2)
                                           (point-position other-paint-point)
                                         (draw-line* stream
                                                     (+ nx1 (- x startx))
                                                     (+ ny1 (- y starty))
                                                     nx2
                                                     ny2
                                                     :line-thickness 4
                                                     :ink *drag-color*))))))))
                    (with-output-to-output-record (stream)
                      (multiple-value-bind (x1 y1)
                          (point-position paint-point)
                        (draw-circle* stream
                                      (+ x1 (- x startx))
                                      (+ y1 (- y starty))
                                      6
                                      :ink ink :filled t)
                        (connect-neighbors paint-point))))))
            ;; FIXME! probably want a better API here
            (with-accessors ((point %point)) paint-point
              (with-accessors ((x1 point-x) (y1 point-y) (%p %point)) point
                (setf point (make-point (+ x1 (- x startx))
                                        (+ y1 (- y starty))))))))))))

;;; 2. com-move-point
(define-clim-paint-command (com-move-point)
    ((presentation presentation))
  (let ((point (presentation-object presentation)))
    (com-drag-move-point point)))

(define-gesture-name move-point-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-point-translator
    (paint-point com-move-point clim-paint
           :gesture move-point-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-point-p object)))
    (object presentation)
  (list presentation))

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
                  (when ink `(:ink ,ink)))))))))

(define-gesture-name add-point-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator point-dragging-add-translator
    (paint-point com-drag-add-point clim-paint
                 :gesture add-point-gesture
                 :menu nil
                 :tester ((object presentation event)
                          (declare (ignore presentation event))
                          (paint-point-p object)))
    (object)
  (list object))

