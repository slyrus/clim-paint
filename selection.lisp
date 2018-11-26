
(in-package :clim-paint)

;;; selection

(define-clim-paint-command (com-select-object)
    ((presentation t))
  (let ((pane (get-frame-pane *application-frame* 'app)))
    (funcall-presentation-generic-function select-presentation
                                           (type-of (presentation-object presentation))
                                           presentation pane :select)))

(define-gesture-name select-gesture :pointer-button (:middle))

(define-presentation-to-command-translator select-object-translator
    (paint-object com-select-object clim-paint
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
