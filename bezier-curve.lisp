
(in-package :clim-paint)

(defclass paint-bezier-curve (paint-object)
  ((bezier-curve :initarg :bezier-curve :accessor %bezier-curve)
   (control-points :initarg :control-points :accessor %control-points)
   (line-thickness :initarg :line-thickness :accessor line-thickness :initform 1)
   (filled :initarg :filled :accessor filledp :initform nil)))

(defmacro control-point (bezier-curve index)
  `(flexichain:element* (%control-points ,bezier-curve) ,index))

(defun control-point-count (bezier-curve)
  (flexichain:nb-elements (%control-points bezier-curve)))

(defun update-bezier-curve (bezier-curve)
  (let ((seq (loop for i below (control-point-count bezier-curve)
                collect (control-point bezier-curve i))))
    (setf (%bezier-curve bezier-curve)
          (make-bezier-curve seq))))

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
         :control-points (make-instance 'flexichain:standard-flexichain
                                        :initial-contents point-seq)
         (append
          (when ink-supplied-p `(:ink , ink))
          (when filled-supplied-p `(:filled ,filled))
          (when line-thickness-supplied-p `(:line-thickness ,line-thickness)))))

;;;
;;; bezier-curve-presentation
(defclass bezier-curve-presentation (standard-presentation) ())

(define-presentation-type bezier-curve-presentation ())

(defclass bezier-curve-handle-point (selection-handle-point)
  ((index :initarg :index :accessor control-points-index)))


(defun draw-bezier-curve-selection (pane bezier-curve &key (ink *selection-color*)
                                                           (filled nil))
  (loop for p1 = nil then p0
     for i from 0 below (control-point-count bezier-curve)
     for p0 = (control-point bezier-curve i)
     do (present (make-instance 'bezier-curve-handle-point
                                :paint-object bezier-curve
                                :point p0
                                :index i
                                :ink ink
                                :radius 5
                                :filled filled)
                 'bezier-curve-handle-point
                 :record-type 'selection-handle-point-presentation
                 :single-box t)
       (when p1
         (draw-line pane p0 p1 :ink +black+ :line-dashes t))))

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
;;; dragging / moving
(defmethod move-dragging ((bezier-curve paint-bezier-curve) stream dx dy)
  (with-output-to-output-record (stream)
    (with-translation (stream dx dy)
      (with-accessors ((ink ink)
                       (line-thickness line-thickness)
                       (filled filledp))
          bezier-curve
        (apply #'draw-bezier-design* stream (%bezier-curve bezier-curve)
               :filled filled
               :ink ink
               (append
                (when line-thickness `(:line-thickness ,line-thickness))))))))

(defmethod move-update ((bezier-curve paint-bezier-curve) dx dy)
  (with-accessors ((ink ink)
                   (line-thickness line-thickness)
                   (filled filledp))
      bezier-curve
    (loop for i from 0 below (control-point-count bezier-curve)
       do 
         (setf (control-point bezier-curve i)
               (multiple-value-bind (x y)
                   (point-position (control-point bezier-curve i))
                 (make-point (+ x dx) (+ y dy)))))
    (update-bezier-curve bezier-curve)))


;;; 3. selection handle dragging
(define-clim-paint-command (com-drag-move-bezier-curve-selection-handle)
    ((bezier-curve-handle-point bezier-curve-handle-point))
  (with-accessors ((bezier-curve paint-object)
                   (index control-points-index))
      bezier-curve-handle-point
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
                  (let ((dx (- x startx))
                        (dy (- y starty)))
                    (with-output-to-output-record (stream)
                      (multiple-value-bind (x0 y0)
                          (point-position (control-point bezier-curve index))
                        (draw-circle* stream
                                        (+ x0 dx)
                                        (+ y0 dy)
                                        (radius bezier-curve-handle-point)
                                        :ink (ink bezier-curve-handle-point)
                                        :filled (filledp bezier-curve-handle-point)
                                        :line-thickness 2))))))
            ;; FIXME! probably want a better API here
            (let ((dx (- x startx))
                  (dy (- y starty)))
              (multiple-value-bind (x0 y0)
                  (point-position (control-point bezier-curve index))
                (setf (control-point bezier-curve index)
                      (make-point (+ x0 dx) (+ y0 dy))))
              (update-bezier-curve bezier-curve))))))))

;;; 4. com-move-bezier-curve-selection-handle
(define-clim-paint-command (com-move-bezier-curve-selection-handle)
    ((presentation presentation))
  (let ((object (presentation-object presentation)))
    (com-drag-move-bezier-curve-selection-handle object)))

(define-gesture-name move-bezier-curve-selection-handle-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator move-bezier-curve-selection-handle-translator
    (bezier-curve-handle-point com-move-bezier-curve-selection-handle clim-paint
                            :gesture move-bezier-curve-selection-handle-gesture
                            :menu nil
                            #+nil :tester
                            #+nil ((object presentation event)
                                   (declare (ignore presentation event))
                                   (selection-handle-object-p object)))
    (object presentation)
  (list presentation))
