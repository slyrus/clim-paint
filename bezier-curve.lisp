
(in-package :clim-paint)

(defclass paint-bezier-curve-segment (paint-object)
  ((segment :initarg :segment :accessor segment)
   (index :initarg :index :accessor segment-index)
   (line-thickness :initarg :line-thickness :accessor line-thickness :initform 1)))

(defclass paint-bezier-curve (paint-object)
  ((control-points :initarg :control-points :accessor %control-points)
   (line-thickness :initarg :line-thickness :accessor line-thickness :initform 3)
   (filled :initarg :filled :accessor filledp :initform nil)))

(defmacro control-point (bezier-curve index)
  `(flexichain:element* (%control-points ,bezier-curve) ,index))

(defun control-point-count (bezier-curve)
  (flexichain:nb-elements (%control-points bezier-curve)))

(defun %make-bezier-curve (bezier-curve)
  (make-bezier-curve
   (loop for i below (control-point-count bezier-curve)
      collect (control-point bezier-curve i))))

(defgeneric paint-bezier-curve-p (object)
  (:method ((object t)) nil)
  (:method ((object paint-bezier-curve)) t)
  (:documentation "Checking for class paint-bezier-curve"))

(defun make-paint-bezier-curve (point-seq
		                &key (ink nil ink-supplied-p)
                                     (filled nil filled-supplied-p)
                                     (line-thickness nil line-thickness-supplied-p))
  (apply #'make-instance 'paint-bezier-curve
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

(defclass bezier-curve-segment-presentation (standard-presentation) ())

(define-presentation-type bezier-curve-segment-presentation ())

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
         (draw-line pane p0 p1 :ink *foreground-color* :line-dashes t))))

(defun draw-paint-bezier-curve-segment (pane bezier-curve-segment)
  (with-accessors ((segment segment)
                   (ink ink)
                   (line-thickness line-thickness))
      bezier-curve-segment
    (apply #'draw-bezier-design* pane (make-bezier-curve segment)
           :ink ink
           (append
            (when line-thickness `(:line-thickness ,line-thickness))))))

(define-presentation-method present (bezier-curve-segment (type paint-bezier-curve-segment) pane
                                                          (view clim-paint-view) &key)
  (draw-paint-bezier-curve-segment pane bezier-curve-segment))

(defun make-bezier-curve-segments (bezier-curve)
  (destructuring-bind (leftover segment-seq)
      (reduce (lambda (acc point)
                (destructuring-bind (build vec)
                    acc
                  (if (= (length build) 3)
                      (progn
                        (vector-push-extend
                         (list 
                          (third build)
                          (second build)
                          (first build)
                          point)
                         vec)
                        (list (list point) vec))
                      (list (cons point build) vec))))
              (loop for i below (control-point-count bezier-curve)
                 collect (control-point bezier-curve i))
              :initial-value (list nil (make-array 4 :fill-pointer 0)))
    (unless (equal (length leftover) 1)
      (error "Invalid point-seq: ~S ~S" bezier-curve leftover))
    segment-seq))

(defun draw-paint-bezier-curve (pane bezier-curve &key ink line-thickness)
  (declare (ignore pane))
  (loop for seg across (make-bezier-curve-segments bezier-curve)
     for i from 0
     do (present (make-instance 'paint-bezier-curve-segment
                                :segment seg
                                :index i
                                :ink ink
                                :line-thickness line-thickness)
                 'paint-bezier-curve-segment
                 :record-type 'bezier-curve-segment-presentation)))

(define-presentation-method present (bezier-curve (type paint-bezier-curve) pane
                                             (view clim-paint-view) &key)
  (with-accessors ((ink ink)
                   (line-thickness line-thickness)
                   (filled filledp))
      bezier-curve
    (draw-paint-bezier-curve pane bezier-curve :ink ink :line-thickness line-thickness))
  (if (gethash bezier-curve *selected-object-hash*)
      (draw-bezier-curve-selection pane bezier-curve)))

;;;
;;; refined-position test
(define-presentation-method presentation-refined-position-test
    ((type paint-bezier-curve-segment) (record bezier-curve-segment-presentation) x y)
  (let ((bezier-curve-segment (presentation-object record)))
    (let ((squared-distance (cdr (apply #'rough-closest-point-on-bezier-curve
                                        (make-point x y)
                                        (segment bezier-curve-segment)))))
      (< squared-distance 100))))

(define-presentation-method presentation-refined-position-test
    ((type paint-bezier-curve) (record bezier-curve-presentation) x y)
  nil)

;;;
;;; highlighting
(define-presentation-method highlight-presentation
    ((type paint-bezier-curve-segment) (record bezier-curve-segment-presentation) stream state)
  (let ((parent (output-record-parent record)))
    (funcall-presentation-generic-function highlight-presentation
                                           (type-of (presentation-object parent))
                                           parent
                                           stream
                                           state)))

(define-presentation-method highlight-presentation
    ((type paint-bezier-curve) (record bezier-curve-presentation) stream state)
  (let ((bezier-curve (presentation-object record)))
    (case state
      (:highlight
       (draw-paint-bezier-curve stream bezier-curve :ink *highlight-color*))
      (:unhighlight
       (draw-paint-bezier-curve stream bezier-curve)))))

;;;
;;; dragging / moving
(defmethod move-dragging ((bezier-curve paint-bezier-curve) stream dx dy)
  (with-output-to-output-record (stream)
    (with-translation (stream dx dy)
      (draw-paint-bezier-curve stream bezier-curve))))

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
    #+nil (update-bezier-curve bezier-curve)))

;;;
;;; selection handle dragging / moving
(defmethod move-dragging ((bezier-curve-handle-point bezier-curve-handle-point) stream dx dy)
  (with-accessors ((bezier-curve paint-object)
                   (index control-points-index))
      bezier-curve-handle-point
    (with-output-to-output-record (stream)
      (multiple-value-bind (x0 y0)
          (point-position (control-point bezier-curve index))
        (draw-circle* stream
                      (+ x0 dx)
                      (+ y0 dy)
                      (radius bezier-curve-handle-point)
                      :ink (ink bezier-curve-handle-point)
                      :filled (filledp bezier-curve-handle-point)
                      :line-thickness 2)))))

(defmethod move-update ((bezier-curve-handle-point bezier-curve-handle-point) dx dy)
  (with-accessors ((bezier-curve paint-object)
                   (index control-points-index))
      bezier-curve-handle-point
    (multiple-value-bind (x0 y0)
        (point-position (control-point bezier-curve index))
      (setf (control-point bezier-curve index)
            (make-point (+ x0 dx) (+ y0 dy))))
    #+nil (update-bezier-curve bezier-curve)))


;;; split a bezier curve

(define-clim-paint-command (com-drag-split-bezier-curve-segment)
    ((bezier-curve-segment paint-bezier-curve-segment) (presentation presentation) (frame frame))
  (with-accessors ((ink ink))
      frame
    (let ((pane (get-frame-pane frame 'app))
          (new-ink (or (ink bezier-curve-segment) ink)))
      (multiple-value-bind (startx starty)
          (stream-pointer-position pane)
        (let ((start-point (make-point startx starty)))
          (let ((nearest-point (apply #'find-closest-point-on-bezier-curve start-point
                                      (segment bezier-curve-segment))))
            (draw-circle pane nearest-point 6
                         :ink +yellow+ :filled t)
            (multiple-value-bind (x y)
                (dragging-output*
                    (pane :finish-on-release t)
                  (lambda (stream x y)
                    (with-output-to-output-record (stream)
                      (draw-circle* stream x y 6
                                    :ink new-ink :filled t))))
              (let ((new-point (make-point x y)))
                (let ((bezier-curve (presentation-object (output-record-parent presentation)))
                      (index (segment-index bezier-curve-segment)))
                  (when bezier-curve
                    (with-accessors
                          ((control-points %control-points))
                        bezier-curve
                      (let ((left-control (control-point bezier-curve (+ 1 (* index 3))))
                            (right-control (control-point bezier-curve (+ 2 (* index 3)))))
                        (flexichain:insert-vector* control-points
                                                   (+ 2 (* index 3))
                                                   (vector (midpoint
                                                            left-control
                                                            new-point)
                                                           new-point
                                                           (midpoint
                                                            new-point
                                                            right-control)))))))))))))))

;;; com-split-line
(define-clim-paint-command (com-split-bezier-curve-segment)
    ((presentation t))
  (let ((bezier-curve-segment (presentation-object presentation)))
    (com-drag-split-bezier-curve-segment bezier-curve-segment presentation *application-frame*)))

(define-gesture-name split-bezier-curve-segment-gesture :pointer-button (:middle :control))

(define-presentation-to-command-translator split-bezier-curve-segment-translator
    (paint-bezier-curve-segment com-split-bezier-curve-segment clim-paint
          :gesture split-bezier-curve-segment-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))



;;; need to pass segment events up to parent!

(define-presentation-to-command-translator move-bezier-segment-translator
    (paint-bezier-curve-segment com-move-object clim-paint
           :gesture move-object-gesture
           :menu nil
           :tester ((object presentation event)
                    (declare (ignore presentation event))
                    (paint-object-p object)))
    (object presentation)
  (list (output-record-parent presentation)))

(define-presentation-to-command-translator select-bezier-segment-translator
    (paint-bezier-curve-segment com-select-object clim-paint
                  :gesture select-gesture
                  :menu nil
                  :tester ((object)
                           t))
    (object presentation)
  (list (output-record-parent presentation)))
