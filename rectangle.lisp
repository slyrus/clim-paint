
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
                          :ink *highlight-color* :filled nil)))
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
                    (make-rectangle* (- x1 *rectangle-highlight-margin* 1)
                                     (- y1 *rectangle-highlight-margin* 1)
                                     (+ x2 *rectangle-highlight-margin* 1)
                                     (+ y2 *rectangle-highlight-margin* 1))))))))))
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
