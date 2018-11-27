
(in-package :clim-paint)

(defparameter *ellipse-highlight-margin* 6)
(defparameter *ellipse-highlight-thickness* 2)

;;;
;;; we want an ellipse object that we can modify but the
;;; standard-ellipse is immutable (?) and itself is a subclass of
;;; elliptical-thing. The problem with elliptical-thing is that it
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
   (line-thickness :initarg :line-thickness :accessor line-thickness :initform 1)
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
                                (filled nil filled-supplied-p)
                                (line-thickness nil line-thickness-supplied-p))
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
                  (when filled-supplied-p `(:filled ,filled))
                  (when line-thickness-supplied-p `(:line-thickness ,line-thickness)))))

;;;
;;; ellipse-presentation
(defclass ellipse-presentation (standard-presentation) ())

(define-presentation-type ellipse-presentation ())

(defclass ellipse-radius-handle-point (selection-handle-point)
  ((radius-index :initarg :radius-index :accessor radius-index
                 :documentation "Either 1 or 2 indicating whether this
                 point corresponds to ellipse radius-1 or radius-2.")
   (radius-dx :initarg :radius-dx :accessor radius-dx)
   (radius-dy :initarg :radius-dy :accessor radius-dy)))

(defclass ellipse-center-handle-point (selection-handle-point)
  ((center-point :initarg :center-point :accessor center-point)))

(defparameter *ellipse-selection-width* 8)

(defun draw-ellipse-selection (pane ellipse &key (ink *selection-color*)
                                                 (radius 5)
                                                 (filled nil))
  (with-accessors ((center-point center-point)
                   (radius-1-dx radius-1-dx)
                   (radius-1-dy radius-1-dy)
                   (radius-2-dx radius-2-dx)
                   (radius-2-dy radius-2-dy)
                   (start-angle start-angle)
                   (end-angle end-angle)
                   (line-thickness line-thickness)
                   (filledp filledp))
      ellipse
    (multiple-value-bind (x1 y1)
        (point-position center-point)
      (let ((theta1 (phase (complex radius-1-dy radius-1-dx)))
            (theta2 (phase (complex radius-2-dy radius-2-dx))))
        (draw-ellipse* pane
                       x1 y1
                       (+ radius-1-dx (* *ellipse-highlight-margin*
                                         (sin theta1)))
                       (+ radius-1-dy (* *ellipse-highlight-margin*
                                         (cos theta1)))
                       (+ radius-2-dx (* *ellipse-highlight-margin*
                                         (sin theta2)))
                       (+ radius-2-dy (* *ellipse-highlight-margin*
                                         (cos theta2)))
                       :ink *selection-color*
                       :line-thickness 2
                       :line-dashes t
                       :filled nil)

        (present (make-instance 'ellipse-center-handle-point
                                :paint-object ellipse
                                :center-point center-point
                                :point center-point
                                :ink ink
                                :radius radius
                                :filled filled)
                 'selection-handle-point
                 :record-type 'selection-handle-point-presentation
                 :single-box t)
        (draw-line* pane
                    x1 y1
                    (+ x1 radius-1-dx)
                    (+ y1 radius-1-dy)
                    :ink *selection-color*
                    :line-thickness 3)
        (present (make-instance 'ellipse-radius-handle-point
                                :paint-object ellipse
                                :radius-index 1
                                :radius-dx radius-1-dx
                                :radius-dy radius-1-dy
                                :point (multiple-value-bind (cx cy)
                                           (point-position center-point)
                                         (make-point (+ cx radius-1-dx) (+ cy radius-1-dy)))
                                :ink ink
                                :radius radius
                                :filled filled)
                 'selection-handle-point
                 :record-type 'selection-handle-point-presentation
                 :single-box t)
        (draw-line* pane
                    x1 y1
                    (+ x1 radius-2-dx)
                    (+ y1 radius-2-dy)
                    :ink *selection-color*
                    :line-thickness 3)
        (present (make-instance 'ellipse-radius-handle-point
                                :paint-object ellipse
                                :radius-index 2
                                :radius-dx radius-2-dx
                                :radius-dy radius-2-dy
                                :point (multiple-value-bind (cx cy)
                                           (point-position center-point)
                                         (make-point (+ cx radius-2-dx) (+ cy radius-2-dy)))
                                :ink ink
                                :radius radius
                                :filled filled)
                 'selection-handle-point
                 :record-type 'selection-handle-point-presentation
                 :single-box t)))))

(define-presentation-method present (ellipse (type paint-ellipse) pane
                                             (view clim-paint-view) &key)
  (with-accessors ((center-point center-point)
                   (radius-1-dx radius-1-dx)
                   (radius-1-dy radius-1-dy)
                   (radius-2-dx radius-2-dx)
                   (radius-2-dy radius-2-dy)
                   (start-angle start-angle)
                   (end-angle end-angle)
                   (line-thickness line-thickness)
                   (filledp filledp)
                   (ink ink))
      ellipse
    (multiple-value-bind (x1 y1)
        (point-position center-point)
      (apply #'draw-ellipse* pane
             x1 y1
             radius-1-dx radius-1-dy
             radius-2-dx radius-2-dy
             :filled filledp
             :ink ink
             (append
              (when start-angle `(:start-angle ,start-angle))
              (when end-angle `(:end-angle ,end-angle))
              (when line-thickness `(:line-thickness ,line-thickness)))))
    (if (gethash ellipse *selected-object-hash*)
        (draw-ellipse-selection pane ellipse))))

;;;
;;; refined-position test
;;;
;;; In theory we could check to see if this is a filled ellipse and,
;;; if not, only hit the ellipse if we're on the perimeter, but for
;;; the moment we don't do that.
(define-presentation-method presentation-refined-position-test
    ((type paint-ellipse) (record ellipse-presentation) x y)
  (let ((ellipse (presentation-object record)))
    (with-accessors ((center-point center-point)
                     (radius-1-dx radius-1-dx)
                     (radius-1-dy radius-1-dy)
                     (radius-2-dx radius-2-dx)
                     (radius-2-dy radius-2-dy)
                     (start-angle start-angle)
                     (end-angle end-angle))
        ellipse
      (multiple-value-bind (center-x center-y)
          (point-position center-point)
        (region-contains-position-p
         (make-ellipse* center-x center-y
                        radius-1-dx radius-1-dy
                        radius-2-dx radius-2-dy
                        :start-angle start-angle
                        :end-angle end-angle)
         x y)))))

;;;
;;; highlighting
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
           (let ((theta1 (phase (complex radius-1-dy radius-1-dx)))
                 (theta2 (phase (complex radius-2-dy radius-2-dx))))
             (draw-ellipse* stream
                            x1 y1
                            (+ radius-1-dx (* *ellipse-highlight-margin*
                                              (sin theta1)))
                            (+ radius-1-dy (* *ellipse-highlight-margin*
                                              (cos theta1)))
                            (+ radius-2-dx (* *ellipse-highlight-margin*
                                              (sin theta2)))
                            (+ radius-2-dy (* *ellipse-highlight-margin*
                                              (cos theta2)))
                            :line-thickness *ellipse-highlight-thickness*
                            :start-angle start-angle
                            :end-angle end-angle
                            :ink *highlight-color*
                            :filled nil)))))
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
                                   (ink ink)
                                   (line-thickness line-thickness))
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
                                     :filled filled
                                     :line-thickness line-thickness))))))
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
