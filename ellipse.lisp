
(in-package :clim-paint)

;;;
;;; we want an ellipse object that we can modify but the
;;; standard-ellipse is impaint and itself is a subclass of
;;; elliptical-thing. The problem with elliptical-thiing is that it
;;; stores the transformation of the unit circle to the ellipse, not
;;; the underlying parameters used to define the ellipse via
;;; make-ellipse. So, we'll store those parameters here, along with a
;;; clim:ellipse (presumably a standard-ellipse) that gets regenerated
;;; when the ellipse paramters change.
(defclass paint-ellipse (ellipse paint-object)
  ((center-point :initarg :center-point :accessor center-point)
   (radius-1-dx :initarg :radius-1-dx :accessor radius-1-dx)
   (radius-1-dy :initarg :radius-1-dy :accessor radius-1-dy)
   (radius-2-dx :initarg :radius-2-dx :accessor radius-2-dx)
   (radius-2-dy :initarg :radius-2-dy :accessor radius-2-dy)
   (start-angle :initarg :start-angle :accessor start-angle :initform 0)
   (end-angle :initarg :end-angle :accessor end-angle :initform (* pi 2))
   (filled :initarg :filled :accessor filledp :initform nil)))

(defgeneric paint-ellipse-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-ellipse)) t)
  (:documentation "Checking for class paint-ellipse"))

(defun make-paint-ellipse (center-point
		           radius-1-dx radius-1-dy
		           radius-2-dx radius-2-dy
		           &key (start-angle nil start-angle-supplied-p)
                                (end-angle nil end-angle-supplied-p)
                                (ink nil ink-supplied-p)
                                (filled nil filled-supplied-p))
  (apply #'make-instance 'paint-ellipse
                 :center-point center-point
                 :radius-1-dx radius-1-dx
                 :radius-1-dy radius-1-dy
	         :radius-2-dx radius-2-dx
                 :radius-2-dy radius-2-dy
                 (append
                  (when start-angle-supplied-p `(:start-angle ,start-angle))
                  (when end-angle-supplied-p `(:end-angle ,end-angle))
                  (when ink-supplied-p `(:ink , ink))
                  (when filled-supplied-p `(:filled ,filled)))))

;;;
;;; ellipse-presentation
(defclass ellipse-presentation (standard-presentation) ())

(define-presentation-type ellipse-presentation ())

(define-presentation-method present (ellipse (type paint-ellipse) pane
                                             (view clim-paint-view) &key)
  (with-accessors ((center-point center-point)
                   (radius-1-dx radius-1-dx)
                   (radius-1-dy radius-1-dy)
                   (radius-2-dx radius-2-dx)
                   (radius-2-dy radius-2-dy)
                   (start-angle start-angle)
                   (end-angle end-angle)
                   (filledp filledp)
                   (ink ink))
      ellipse
    (multiple-value-bind (x1 y1)
        (point-position center-point)
      (draw-ellipse* pane
                     x1 y1
                     radius-1-dx radius-1-dy
                     radius-2-dx radius-2-dy
                     :start-angle start-angle
                     :end-angle end-angle
                     :filled filledp
                     :ink ink))))

;;;
;;; highlighting
(defparameter *ellipse-highlight-margin* 4)

(define-presentation-method highlight-presentation
    ((type paint-ellipse) (record ellipse-presentation) stream state)
  (let ((paint-ellipse (presentation-object record)))
    (case state
      (:highlight
       (with-accessors ((center-point center-point)
                        (radius-1-dx radius-1-dx)
                        (radius-1-dy radius-1-dy)
                        (radius-2-dx radius-2-dx)
                        (radius-2-dy radius-2-dy)
                        (start-angle start-angle)
                        (end-angle end-angle)
                        (filledp filledp)
                        (ink ink))
           paint-ellipse
         (multiple-value-bind (x1 y1)
             (point-position center-point)
           (draw-ellipse* stream
                          x1 y1
                          (+ radius-1-dx *ellipse-highlight-margin*)
                          (+ radius-1-dy *ellipse-highlight-margin*)
                          (+ radius-2-dx *ellipse-highlight-margin*)
                          (+ radius-2-dy *ellipse-highlight-margin*)
                          :line-thickness *ellipse-highlight-margin*
                          :start-angle start-angle
                          :end-angle end-angle
                          :ink *highlight-color*
                          :filled nil))))
      (:unhighlight
       (queue-repaint
        stream
        (make-instance
         'window-repaint-event
         :sheet stream
         :region (transform-region
                  (sheet-native-transformation stream)
                  (with-accessors ((center-point center-point)
                                   (radius-1-dx radius-1-dx)
                                   (radius-1-dy radius-1-dy)
                                   (radius-2-dx radius-2-dx)
                                   (radius-2-dy radius-2-dy))
                      paint-ellipse
                    (multiple-value-bind (x1 y1)
                        (point-position center-point)
                      (make-ellipse* x1 y1
                                     (+ radius-1-dx *ellipse-highlight-margin* 1)
                                     (+ radius-1-dy *ellipse-highlight-margin* 1)
                                     (+ radius-2-dx *ellipse-highlight-margin* 1)
                                     (+ radius-2-dy *ellipse-highlight-margin* 1)))))))))))

;;; 1. com-drag-move-ellipse
(define-clim-paint-command (com-drag-move-ellipse)
    ((paint-ellipse paint-ellipse))
  (with-accessors ((ink ink)
                   (filled filledp))
      paint-ellipse
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (multiple-value-bind (startx starty)
          (stream-pointer-position pane)
        (multiple-value-bind (x y)
            (dragging-output*
                (pane :finish-on-release t)
              (lambda (stream x y)
                (with-output-to-output-record (stream)
                  (with-accessors ((center-point center-point)
                                   (radius-1-dx radius-1-dx)
                                   (radius-1-dy radius-1-dy)
                                   (radius-2-dx radius-2-dx)
                                   (radius-2-dy radius-2-dy)
                                   (start-angle start-angle)
                                   (end-angle end-angle)
                                   (filledp filledp)
                                   (ink ink))
                      paint-ellipse
                    (multiple-value-bind (x1 y1)
                        (point-position center-point)
                      (draw-ellipse* stream
                                     (+ x1 (- x startx))
                                     (+ y1 (- y starty))
                                     radius-1-dx
                                     radius-1-dy
                                     radius-2-dx
                                     radius-2-dy
                                     :start-angle start-angle
                                     :end-angle end-angle
                                     :ink ink
                                     :filled filled))))))
          ;; FIXME! probably want a better API here
          (with-accessors ((center-point center-point))
              paint-ellipse
            (multiple-value-bind (x1 y1)
                (point-position center-point)
              (setf center-point (make-point
                                  (+ x1 (- x startx))
                                  (+ y1 (- y starty)))))))))))

;;; 2. com-move-ellipse
(define-clim-paint-command (com-move-ellipse)
    ((presentation presentation))
  (let ((ellipse (presentation-object presentation)))
    (com-drag-move-ellipse ellipse)))

(define-gesture-name move-ellipse-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-ellipse-translator
    (paint-ellipse com-move-ellipse clim-paint
           :gesture move-ellipse-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-ellipse-p object)))
    (object presentation)
  (list presentation))
