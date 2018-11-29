
(in-package :clim-paint)

(defclass paint-object ()
  ((ink :initarg :ink :accessor ink)
   (filled :initarg :filled :accessor filledp)))

(defgeneric paint-object-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-object)) t)
  (:documentation "Checking for class paint-object"))

;;;
;;; some special variables to be used for drawing/dragging
(defparameter *highlight-color* +orange+)

(defparameter *selection-color* +red+)

(defparameter *drag-color* +green+)

(defparameter *selected-object-hash* (make-hash-table))

(defgeneric move-dragging (paint-object stream dx dy))

(defgeneric move-update (paint-object dx dy))

;;; moving objects
(define-clim-paint-command (com-drag-move-object)
    ((object paint-object))
  (let ((pane (get-frame-pane *application-frame* 'app)))
    (multiple-value-bind (startx starty)
        (stream-pointer-position pane)
      (multiple-value-bind (x y)
          (dragging-output*
              (pane :finish-on-release t)
            (lambda (stream x y)
              (move-dragging object stream (- x startx) (- y starty))))
        (move-update object (- x startx) (- y starty))))))

(define-clim-paint-command (com-move-object)
    ((presentation presentation))
  (let ((object (presentation-object presentation)))
    (com-drag-move-object object)))

(define-gesture-name move-object-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-object-translator
    (paint-object com-move-object clim-paint
           :gesture move-object-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-object-p object)))
    (object presentation)
  (list presentation))

;;; moving object selection handles
(define-clim-paint-command (com-move-selection-handle-object)
    ((presentation presentation))
  (let ((object (presentation-object presentation)))
    (com-drag-move-object object)))

(define-gesture-name move-selection-handle-object-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator move-selection-handle-object-translator
    (selection-handle-object com-move-selection-handle-object clim-paint
                             :gesture move-selection-handle-object-gesture
                             :menu nil
                             :tester
                             ((object presentation event)
                              (declare (ignore presentation event))
                              (selection-handle-object-p object)))
    (object presentation)
  (list presentation))
