
(in-package :clim-paint)

(defclass paint-bezier-curve (paint-object)
  ((bezier-curve :initarg :bezier-curve :accessor %bezier-curve)
   (line-thickness :initarg :line-thickness :accessor line-thickness :initform 1)
   (filled :initarg :filled :accessor filledp :initform nil)))

(defgeneric paint-bezier-curve-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-bezier-curve)) t)
  (:documentation "Checking for class paint-bezier-curve"))

(defun make-paint-bezier-curve (point-seq
		                &key (ink nil ink-supplied-p)
                                     (filled nil filled-supplied-p)
                                     (line-thickness nil line-thickness-supplied-p))
  (apply #'make-instance 'paint-bezier-curve
         :bezier-curve (make-bezier-curve point-seq)
         (append
          (when ink-supplied-p `(:ink , ink))
          (when filled-supplied-p `(:filled ,filled))
          (when line-thickness-supplied-p `(:line-thickness ,line-thickness)))))

;;;
;;; bezier-curve-presentation
(defclass bezier-curve-presentation (standard-presentation) ())

(define-presentation-type bezier-curve-presentation ())

(defun draw-bezier-curve-selection (pane bezier-curve &key (ink *selection-color*)
                                                           (filled nil))
  (let* ((curve (%bezier-curve bezier-curve))
         (segments (mcclim-bezier::%segments curve)))
    (map nil (lambda (segment)
               (draw-line pane
                          (slot-value segment 'mcclim-bezier::p0)
                          (slot-value segment 'mcclim-bezier::p1)
                          :ink +black+)
               (draw-line pane
                          (slot-value segment 'mcclim-bezier::p1)
                          (slot-value segment 'mcclim-bezier::p2)
                          :ink +black+)
               (draw-line pane
                          (slot-value segment 'mcclim-bezier::p2)
                          (slot-value segment 'mcclim-bezier::p3)
                          :ink +black+))
            segments)))

(define-presentation-method present (bezier-curve (type paint-bezier-curve) pane
                                             (view clim-paint-view) &key)
  (with-accessors ((curve %bezier-curve)
                   (line-thickness line-thickness)
                   (filledp filledp)
                   (ink ink))
      bezier-curve
    (apply #'draw-bezier-design* pane curve
           :filled filledp
           :ink ink
           (append
            (when line-thickness `(:line-thickness ,line-thickness))))
    (if (gethash bezier-curve *selected-object-hash*)
        (draw-bezier-curve-selection pane bezier-curve))))

;;;
;;; Can't we abstract this away to a single class somehow??
;;;

;;; 1. com-drag-move-rectangle
(define-clim-paint-command (com-drag-move-bezier-curve)
    ((bezier-curve paint-bezier-curve))
  (with-accessors ((ink ink)
                   (line-thickness line-thickness)
                   (filled filledp))
      bezier-curve
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (multiple-value-bind (startx starty)
          (stream-pointer-position pane)
        (multiple-value-bind (x y)
            (dragging-output*
                (pane :finish-on-release t)
              (lambda (stream x y)
                (with-output-to-output-record (stream)
                  (with-translation (stream (- x startx) (- y starty))
                    ;; draw while dragging
                    (apply #'draw-bezier-design* stream (%bezier-curve bezier-curve)
                           :filled filled
                           :ink ink
                           (append
                            (when line-thickness `(:line-thickness ,line-thickness))))))))
          ;; update the bezier values
          (setf (%bezier-curve bezier-curve)
                (transform-region (make-translation-transformation
                                   (- x startx) (- y starty))
                                  (%bezier-curve bezier-curve))))))))

;;; 2. com-move-bezier-curve
(define-clim-paint-command (com-move-bezier-curve)
    ((presentation presentation))
  (let ((bezier-curve (presentation-object presentation)))
    (com-drag-move-bezier-curve bezier-curve)))

(define-gesture-name move-bezier-curve-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-bezier-curve-translator
    (paint-bezier-curve com-move-bezier-curve clim-paint
           :gesture move-bezier-curve-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-bezier-curve-p object)))
    (object presentation)
  (list presentation))

