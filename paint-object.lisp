
(in-package :clim-paint)

(defclass paint-object ()
  ((ink :initarg :ink :accessor ink)
   (filled :initarg :filled :accessor filledp)))

(defgeneric paint-object-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-object)) t)
  (:documentation "Checking for class paint-object"))


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
        (move-update object (- x startx) (- y starty)))
      (setf (pane-needs-redisplay pane) t))
    (redisplay-frame-panes *application-frame*)))

;;;
;;; move
(define-clim-paint-command (com-move-object)
    ((presentation presentation))
  (let ((object (presentation-object presentation)))
    (com-drag-move-object object)
    (redisplay-frame-panes *application-frame*)))

(define-gesture-name move-object-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator move-object-translator
    (paint-object com-move-object clim-paint
           :gesture move-object-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-object-p object)))
    (object presentation)
  (list presentation))

;;;
;;; move and select
(define-clim-paint-command (com-move-and-select-object)
    ((presentation presentation))
  (let ((object (presentation-object presentation)))
    (com-drag-move-object object)
    (let ((pane (get-frame-pane *application-frame* 'app)))
      (funcall-presentation-generic-function select-presentation
                                             (type-of (presentation-object presentation))
                                             presentation pane :select))
    (redisplay-frame-panes *application-frame*)))

(define-gesture-name move-and-select-object-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-and-select-object-translator
    (paint-object com-move-and-select-object clim-paint
           :gesture move-and-select-object-gesture
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
    (com-drag-move-object object)
    (redraw-properties-pane)))

(define-gesture-name move-selection-handle-object-gesture :pointer-button (:left))

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

(defmethod setup-properties-pane ((object paint-object) frame)
  (let ((app-pane (find-pane-named frame 'app))
        (old-properties-pane
         (find-pane 'properties (frame-top-level-sheet frame))))
    (let ((properties-pane (make-properties-pane object)))
      (setf (pane-object properties-pane) object)
      (setf (pane-needs-redisplay app-pane) t)
      (let ((parent (sheet-parent old-properties-pane)))
        (sheet-disown-child parent old-properties-pane :errorp t)
        (sheet-adopt-child parent properties-pane))
      (clim:redisplay-frame-pane frame app-pane))))
