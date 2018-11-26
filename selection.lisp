
(in-package :clim-paint)

;;; selection handle
(defclass selection-handle-object ()
  ((ink :initarg :ink :accessor ink)
   (line-thickness :initarg :line-thickness :accessor line-thickness)
   (filled :initarg :filled :accessor filledp)))

(defclass selection-handle-point (selection-handle-object)
  ((point :type point :initarg :point :accessor %point)
   (radius :initarg :radius :accessor radius)))

;;;
;;; selection-handle-point-presentation
(defclass selection-handle-point-presentation (standard-presentation) ())

(define-presentation-type selection-handle-point-presentation ())

(define-presentation-method present (object (type selection-handle-point) pane
                                            (view clim-paint-view)
                                            &key)
  (multiple-value-bind (x y)
      (point-position (%point object))
    (draw-circle* pane x y (radius object)
                  :ink (ink object)
                  :filled (filledp object)
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
  (case state
    (:select
     (clrhash *selected-object-hash*)
     (let ((object (presentation-object record)))
       (setf (gethash object *selected-object-hash*) t))
     (queue-repaint
      stream
      (make-instance 'window-repaint-event
                     :sheet stream
                     :region +everywhere+)))
    (:deselect
     (clrhash *selected-object-hash*)
     (queue-repaint
      stream
      (make-instance 'window-repaint-event
                     :sheet stream
                     :region +everywhere+)))))

