
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

(define-presentation-type rectangle-presentation ())

(defparameter *rectangle-selection-width* 4)

(defun draw-rectangle-selection (pane rectangle &key (ink +black+)
                                                     (radius 8)
                                                     (filled nil))
  (multiple-value-bind (x1 y1 x2 y2)
      (bounding-rectangle* rectangle)
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
      (map nil
           (lambda (x)
             (present (make-instance 'selection-handle-point
                                     :paint-object rectangle
                                     :point (apply #'make-point x)
                                     :ink ink
                                     :radius radius
                                     :filled filled)
                      'selection-handle-point
                      :record-type 'selection-handle-point-presentation
                      :single-box t))
           (list (list sx1 sy1)
                 (list sx2 sy1)
                 (list sx2 sy2)
                 (list sx1 sy2))))))

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
  (let ((paint-rectangle (presentation-object record)))
    (case state
      (:highlight
       (multiple-value-bind (x1 y1 x2 y2)
        (bounding-rectangle* paint-rectangle)
         (draw-rectangle* stream
                          (- x1 *rectangle-highlight-margin* 1)
                          (- y1 *rectangle-highlight-margin* 1)
                          (+ x2 *rectangle-highlight-margin*)
                          (+ y2 *rectangle-highlight-margin*)
                          :ink *highlight-color*
                          :filled nil
                          :line-thickness *rectangle-highlight-margin*)))
      (:unhighlight
       (queue-repaint
        stream
        (make-instance
         'window-repaint-event
         :sheet stream
         :region (transform-region
                  (sheet-native-transformation stream)
                  (multiple-value-bind (x1 y1 x2 y2)
                      (bounding-rectangle* record)
                    (make-rectangle* (- x1 *rectangle-highlight-margin* 2)
                                     (- y1 *rectangle-highlight-margin* 2)
                                     (+ x2 *rectangle-highlight-margin* 2)
                                     (+ y2 *rectangle-highlight-margin* 2))))))))))
;;; 1. com-drag-move-rectangle
(define-clim-paint-command (com-drag-move-rectangle)
    ((paint-rectangle paint-rectangle))
  (with-accessors ((ink ink)
                   (filled filledp))
      paint-rectangle
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (multiple-value-bind (startx starty)
          (stream-pointer-position pane)
        (multiple-value-bind (x y)
            (dragging-output*
                (pane :finish-on-release t)
              (lambda (stream x y)
                (with-output-to-output-record (stream)
                  (multiple-value-bind (x1 y1 x2 y2)
                      (bounding-rectangle* paint-rectangle)
                    (draw-rectangle* stream
                                     (+ x1 (- x startx))
                                     (+ y1 (- y starty))
                                     (+ x2 (- x startx))
                                     (+ y2 (- y starty))
                                     :ink ink
                                     :filled filled)))))
          ;; FIXME! probably want a better API here
          (with-accessors ((rectangle %rectangle)) paint-rectangle
            (multiple-value-bind (x1 y1 x2 y2)
                (bounding-rectangle* paint-rectangle)
              (setf rectangle (make-rectangle*
                               (+ x1 (- x startx))
                               (+ y1 (- y starty))
                               (+ x2 (- x startx))
                               (+ y2 (- y starty)))))))))))

;;; 2. com-move-rectangle
(define-clim-paint-command (com-move-rectangle)
    ((presentation presentation))
  (let ((rectangle (presentation-object presentation)))
    (com-drag-move-rectangle rectangle)))

(define-gesture-name move-rectangle-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-rectangle-translator
    (paint-rectangle com-move-rectangle clim-paint
           :gesture move-rectangle-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-rectangle-p object)))
    (object presentation)
  (list presentation))


(define-clim-paint-command (com-drag-move-rectangle-selection-handle)
    ((object selection-handle-object))
  (let ((paint-rectangle (paint-object object)))
    (with-accessors ((ink ink)
                     (filled filledp))
        paint-rectangle
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (startx starty)
            (stream-pointer-position pane)
          (multiple-value-bind (x y)
              (dragging-output*
                  (pane :finish-on-release t)
                (lambda (stream x y)
                  (with-output-to-output-record (stream)
                    (multiple-value-bind (x1 y1 x2 y2)
                        (bounding-rectangle* paint-rectangle)
                      (draw-rectangle* stream
                                       (+ x1 (- x startx))
                                       (+ y1 (- y starty))
                                       (+ x2 (- x startx))
                                       (+ y2 (- y starty))
                                       :ink ink
                                       :filled filled)))))
            ;; FIXME! probably want a better API here
            (with-accessors ((rectangle %rectangle)) paint-rectangle
              (multiple-value-bind (x1 y1 x2 y2)
                  (bounding-rectangle* paint-rectangle)
                (setf rectangle (make-rectangle*
                                 (+ x1 (- x startx))
                                 (+ y1 (- y starty))
                                 (+ x2 (- x startx))
                                 (+ y2 (- y starty))))))))))))

;;; 4. com-move-rectangle-selection-handle
(define-clim-paint-command (com-move-rectangle-selection-handle)
    ((presentation presentation))
  (let ((object (presentation-object presentation)))
    (com-drag-move-rectangle-selection-handle object)))

(define-gesture-name move-rectangle-selection-handle-gesture :pointer-button (:middle :control))

(define-presentation-to-command-translator move-rectangle-selection-handle-translator
    (selection-handle-point com-move-rectangle-selection-handle clim-paint
           :gesture move-rectangle-selection-handle-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (selection-handle-object-p object)))
    (object presentation)
  (list presentation))
