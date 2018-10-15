
(in-package :clim-paint)

;;;
;;; paint lines
(defclass paint-line (paint-object)
  ((p1 :type paint-point :initarg :p1)
   (p2 :type paint-point :initarg :p2)))

(defgeneric paint-line-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-line)) t)
  (:documentation "Checking for class paint-line"))

(defun make-paint-line (start-point end-point &key ink)
  (apply #'make-instance 'paint-line :p1 start-point :p2 end-point
         (when ink `(:ink ,ink))))

(defmethod line-start-point* ((line paint-line))
  (with-slots (p1) line
    (with-slots (x y)
        p1
      (values x y))))

(defmethod line-end-point* ((line paint-line))
  (with-slots (p2) line
    (with-slots (x y)
        p2
      (values x y))))

(defmethod line-start-point ((line paint-line))
  (with-slots (p1) line
    p1))

(defmethod line-end-point ((line paint-line))
  (with-slots (p2) line
    p2))

(defun find-lines-containing (point shapes)
  (loop for shape in shapes
     when (and (paint-line-p shape)
               (or (eq point (line-start-point shape))
                   (eq point (line-end-point shape))))
     collect shape))

;;;
;;; line-presentation
(defclass line-presentation (standard-presentation) ())

(define-presentation-type line-presentation ())

(define-presentation-method present (line (type paint-line) pane
                                          (view clim-paint-view) &key)
  (with-accessors ((ink ink))
      line
    (draw-line pane
               (line-start-point line)
               (line-end-point line)
               :ink ink)))

;;;
;;; highlighting
(define-presentation-method highlight-presentation
    ((type paint-line) (record line-presentation) stream state)
  (let ((line (presentation-object record)))
    (with-accessors ((start line-start-point)
                     (end line-end-point))
        line
      (case state
        (:highlight
         (draw-line stream start end
                    :line-thickness 4 :ink *highlight-color*))
        (:unhighlight
         (queue-repaint
          stream
          (make-instance 'window-repaint-event
                         :sheet stream
                         :region (transform-region
                                  (sheet-native-transformation stream)
                                  record))))))))

(define-presentation-method presentation-refined-position-test
    ((type paint-line) (record line-presentation) x y)
  (let ((line (presentation-object record)))
    (line-point-between-p (make-point x y)
                          (line-start-point line)
                          (line-end-point line))))

;;; commands

;;; 1. com-add-line
(define-clim-paint-command (com-add-line :name t)
    ((point1 point :prompt "Point 1")
     (point2 point :prompt "Point 2")
     &key
     (ink color))
  (with-accessors ((shapes shapes)
                   (default-ink ink))
      *application-frame*
    (when (and point1 point2)
      (let ((line (make-paint-line point1 point2 :ink (or ink default-ink))))
        (push line shapes)))))

;;; 2. com-drag-split-line
(define-clim-paint-command (com-drag-split-line)
    ((line paint-line) (presentation presentation) (frame frame))
  (declare (ignore presentation))
  (with-accessors ((ink ink)
                   (shapes shapes))
      frame
    (let ((pane (get-frame-pane frame 'app)))
      (multiple-value-bind (x y)
          (dragging-output (pane :finish-on-release t)
            (draw-circle pane (get-pointer-position pane) 6
                         :ink ink :filled t))
        (let ((p1 (find (line-start-point line) shapes :test 'paint-point=))
              (p2 (find (line-end-point line) shapes :test 'paint-point=)))
          (setf shapes (delete-if (lambda (x)
                                    (and (paint-line-p x)
                                         (or
                                          (and (paint-point= (line-start-point x) p1)
                                               (paint-point= (line-end-point x) p2))
                                          (and (paint-point= (line-start-point x) p2)
                                               (paint-point= (line-end-point x) p1)))))
                                  shapes))
          (let ((new-point (com-add-point x y)))
            (com-add-line p1 new-point)
            (com-add-line p2 new-point)))))))

;;; 3. com-split-line
(define-clim-paint-command (com-split-line)
    ((presentation t))
  (let ((line (presentation-object presentation)))
    (com-drag-split-line line presentation *application-frame*)))

(define-gesture-name split-line-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator split-line-translator
    (paint-line com-split-line clim-paint
          :gesture split-line-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))


;;; 4. com-drag-move-line
(define-clim-paint-command (com-drag-move-line)
    ((line paint-line))
  (with-accessors ((shapes shapes))
      *application-frame*
    (with-accessors ((ink ink))
        line
      (let ((pane (get-frame-pane *application-frame* 'app)))
        (multiple-value-bind (startx starty)
            (stream-pointer-position pane)
          (multiple-value-bind (x y)
              (dragging-output*
                  (pane :finish-on-release t)
                (lambda (stream x y)
                  (flet ((connect-neighbors (paint-point)
                           (let ((neighbors
                                  (remove line (find-lines-containing paint-point shapes))))
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
                      (let ((paint-point-1 (line-start-point line))
                            (paint-point-2 (line-end-point line)))
                        (multiple-value-bind (x1 y1)
                            (point-position paint-point-1)
                          (multiple-value-bind (x2 y2)
                              (point-position paint-point-2)
                            (draw-circle* stream
                                          (+ x1 (- x startx))
                                          (+ y1 (- y starty))
                                          6
                                          :ink ink :filled t)
                            (draw-circle* stream
                                          (+ x2 (- x startx))
                                          (+ y2 (- y starty))
                                          6
                                          :ink ink :filled t)
                            (connect-neighbors paint-point-1)
                            (connect-neighbors paint-point-2)
                            (draw-line* stream
                                        (+ x1 (- x startx))
                                        (+ y1 (- y starty))
                                        (+ x2 (- x startx))
                                        (+ y2 (- y starty))
                                        :line-thickness 4
                                        :ink *drag-color*))))))))
            (let ((paint-point-1 (line-start-point line))
                  (paint-point-2 (line-end-point line)))
              ;; FIXME! probably want a better API here
              (with-accessors ((p1 %point)) paint-point-1
                (with-accessors ((x1 point-x) (y1 point-y)) p1
                  (setf p1 (make-point (+ x1 (- x startx))
                                       (+ y1 (- y starty))))
                  (with-accessors ((p2 %point)) paint-point-2
                    (with-accessors ((x2 point-x) (y2 point-y)) p2
                      (setf p2 (make-point (+ x2 (- x startx))
                                           (+ y2 (- y starty)))))))))))))))
;;; 5. com-move-line
(define-clim-paint-command (com-move-line)
    ((presentation t))
  (with-accessors ((shapes shapes))
      *application-frame*
    (let ((line (presentation-object presentation)))
      (com-drag-move-line line))))

(define-gesture-name move-line-gesture :pointer-button (:left))

(define-presentation-to-command-translator move-line-translator
    (paint-line com-move-line clim-paint
          :gesture move-line-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))


