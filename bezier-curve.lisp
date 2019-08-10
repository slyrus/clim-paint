
(in-package :clim-paint)

(defclass paint-bezier-curve-segment (paint-object)
  ((parent :initarg :parent :accessor parent)
   (segment :initarg :segment :accessor segment)
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

(defun control-points (bezier-curve)
  (loop for i below (control-point-count bezier-curve)
      collect (control-point bezier-curve i)))

(defun %make-bezier-curve (bezier-curve)
  (make-bezier-curve (control-points bezier-curve)))

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

(defclass bezier-curve-point (selection-handle-point)
  ((index :initarg :index :accessor control-points-index)))

(defclass bezier-curve-point-on-curve (bezier-curve-point) ())

(defun draw-bezier-curve-selection (pane bezier-curve &key (ink *selection-color*)
                                                           (filled nil))
  (let ((presentation (stream-current-output-record pane)))
    (loop for segment-presentation across (output-record-children presentation)
       with first = t
       for i from 0 by 3
       for paint-bezier-curve-segment = (presentation-object segment-presentation)
       do
         (draw-paint-bezier-curve-segment pane paint-bezier-curve-segment :ink ink)
         (let ((points (segment paint-bezier-curve-segment)))
           (when first
             (present (make-instance 'bezier-curve-point-on-curve
                                     :paint-object bezier-curve
                                     :point (elt points 0)
                                     :index i
                                     :ink ink
                                     :radius 5
                                     :filled filled)
                      'bezier-curve-point-on-curve
                      :single-box t)
             (setf first nil))
           (draw-line pane (elt points 0) (elt points 1)
                      :ink *foreground-color* :line-dashes t)
           (present (make-instance 'bezier-curve-point
                                     :paint-object bezier-curve
                                     :point (elt points 1)
                                     :index (+ i 1)
                                     :ink ink
                                     :radius 5
                                     :filled filled)
                      'bezier-curve-point
                      :single-box t)
           #+nil
           (draw-line pane (elt points 1) (elt points 2)
                      :ink *foreground-color* :line-dashes t)
           (present (make-instance 'bezier-curve-point
                                     :paint-object bezier-curve
                                     :point (elt points 2)
                                     :index (+ i 2)
                                     :ink ink
                                     :radius 5
                                     :filled filled)
                      'bezier-curve-point
                      :single-box t)
           (draw-line pane (elt points 2) (elt points 3)
                      :ink *foreground-color* :line-dashes t)
           (present (make-instance 'bezier-curve-point-on-curve
                                   :paint-object bezier-curve
                                   :point (elt points 3)
                                   :index (+ i 3)
                                   :ink ink
                                   :radius 5
                                   :filled filled)
                    'bezier-curve-point-on-curve
                    :single-box t)))))
;;;
;;; Need a special presentation-to-command-translator for
;;; bezier-curve-segment here. For now we'll just call
;;; com-move-and-select-object on the parent bezier curve. In theory
;;; could do something more interesting here.
(define-presentation-to-command-translator move-and-select-bezier-curve-segment-translator
    (paint-bezier-curve-segment com-move-and-select-object clim-paint
                  :gesture move-and-select-object-gesture
                  :menu nil
                  :tester ((object presentation event)
                           (declare (ignore presentation event))
                           (paint-object-p object)))
    (object presentation)
  (list (output-record-parent presentation)))

;;; com-delete-bezier-curve-point-on-curve
(define-clim-paint-command (com-delete-bezier-curve-point-on-curve)
    ((presentation t))
  (let ((bezier-curve-point (presentation-object presentation)))
    (let ((bezier-curve (presentation-object (output-record-parent presentation)))
          (index (control-points-index bezier-curve-point)))
      (with-accessors
            ((control-points %control-points))
          bezier-curve
        (if (> index 0)
            (if (= index (1- (control-point-count bezier-curve)))
                (flexichain:delete-elements* control-points
                                             (- index 2)
                                             3)
                (flexichain:delete-elements* control-points
                                             (1- index)
                                             3))
            (flexichain:delete-elements* control-points
                                         0
                                         3))))))

(define-gesture-name delete-bezier-curve-point-on-curve-gesture :pointer-button (:right :shift))

(define-presentation-to-command-translator delete-bezier-curve-point-on-curve-translator
    (bezier-curve-point-on-curve com-delete-bezier-curve-point-on-curve clim-paint
          :gesture delete-bezier-curve-point-on-curve-gesture
          :menu nil
          :tester ((object)
                   t))
    (object presentation)
  (list presentation))


(defun draw-paint-bezier-curve-segment (pane bezier-curve-segment &key ink)
  (with-accessors ((segment segment)
                   (object-ink ink)
                   (line-thickness line-thickness))
      bezier-curve-segment
    (apply #'draw-bezier-design* pane (make-bezier-curve segment)
           :ink (or ink object-ink)
           (append
            (when line-thickness `(:line-thickness ,line-thickness))))))

(define-presentation-method present (bezier-curve-segment
                                     (type paint-bezier-curve-segment) pane
                                     (view clim-paint-view)
                                     &key)
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
                                :parent bezier-curve
                                :segment seg
                                :index i
                                :ink ink
                                :line-thickness line-thickness)
                 'paint-bezier-curve-segment)))

(define-presentation-method present (bezier-curve (type paint-bezier-curve) pane
                                                  (view clim-paint-view) &key)
  (with-accessors ((ink ink)
                   (line-thickness line-thickness)
                   (filled filledp))
      bezier-curve
    (draw-paint-bezier-curve pane bezier-curve :ink ink :line-thickness line-thickness))
  (if (gethash bezier-curve (selected-object-hash *application-frame*))
      (draw-bezier-curve-selection pane bezier-curve)))

;;;
;;; refined-position test
(define-presentation-method presentation-refined-position-test
    ((type paint-bezier-curve-segment) record x y)
  (let ((bezier-curve-segment (presentation-object record)))
    (let ((squared-distance (cdr (apply #'rough-closest-point-on-bezier-curve
                                        (make-point x y)
                                        (segment bezier-curve-segment)))))
      (< squared-distance 50))))

(define-presentation-method presentation-refined-position-test
    ((type paint-bezier-curve) record x y)
  nil)

;;;
;;; highlighting
(define-presentation-method highlight-presentation
    ((type paint-bezier-curve-segment) record stream state)
  (let ((parent (output-record-parent record)))
    (funcall-presentation-generic-function highlight-presentation
                                           (type-of (presentation-object parent))
                                           parent
                                           stream
                                           state)))

(define-presentation-method highlight-presentation
    ((type paint-bezier-curve) record stream state)
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
;;; segment dragging / moving
(defmethod move-dragging ((bezier-curve-segment paint-bezier-curve-segment) stream dx dy)
  (with-output-to-output-record (stream)
    (with-translation (stream dx dy)
      (let ((bezier-curve (parent bezier-curve-segment)))
        (draw-paint-bezier-curve stream bezier-curve)))))

(defmethod move-update ((bezier-curve-segment paint-bezier-curve-segment) dx dy)
  (let ((bezier-curve (parent bezier-curve-segment)))
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
      #+nil (update-bezier-curve bezier-curve))))

;;;
;;; selection handle dragging / moving
(defmethod move-dragging ((bezier-curve-point bezier-curve-point) stream dx dy)
  (with-accessors ((bezier-curve paint-object)
                   (index control-points-index))
      bezier-curve-point
    (with-output-to-output-record (stream)
      (multiple-value-bind (x0 y0)
          (point-position (control-point bezier-curve index))
        (draw-circle* stream
                      (+ x0 dx)
                      (+ y0 dy)
                      (radius bezier-curve-point)
                      :ink (ink bezier-curve-point)
                      :filled (filledp bezier-curve-point)
                      :line-thickness 2)))))

(defmethod move-update ((bezier-curve-point bezier-curve-point) dx dy)
  (with-accessors ((bezier-curve paint-object)
                   (index control-points-index))
      bezier-curve-point
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
                (dragging-output
                    (pane :feedback
                          (lambda (record stream initial-x initial-y x y event)
                            (multiple-value-bind (record-x record-y)
                                (output-record-position record)
                              (let ((dx (- record-x initial-x))
                                    (dy (- record-y initial-y)))
                                (case event
                                  (:draw
                                   (when (output-record-parent record)
                                     (delete-output-record record (output-record-parent record)))
                                   (setf (output-record-position record)
                                         (values (+ dx x) (+ dy y)))
                                   (add-output-record
                                    (with-output-to-output-record (stream)
                                      (draw-circle* stream x y 6
                                                    :ink new-ink :filled t))
                                    record)
                                   (stream-add-output-record stream record)
                                   ;; Question: why do I have to use
                                   ;; +everywhere+ here? Can't I just redraw
                                   ;; the damaged part?
                                   (repaint-sheet stream (bounding-rectangle +everywhere+)))
                                  (:erase
                                   (clear-output-record record)
                                   (repaint-sheet stream (bounding-rectangle record)))))))
                          :finish-on-release t))
              (let ((new-point (make-point x y)))
                (let ((bezier-curve (presentation-object (output-record-parent presentation)))
                      (index (segment-index bezier-curve-segment)))
                  (when bezier-curve
                    (with-accessors
                          ((control-points %control-points))
                        bezier-curve
                      (let ((left-control (control-point bezier-curve (+ 1 (* index 3))))
                            (right-control (control-point bezier-curve (+ 2 (* index 3)))))
                        (flexichain:insert-vector*
                         control-points
                         (+ 2 (* index 3))
                         (vector (add-points
                                  (midpoint
                                   left-control
                                   new-point)
                                  (make-point (/ (- x startx) 2)
                                              (/ (- y starty) 2)))
                                 new-point
                                 (add-points
                                  (midpoint
                                   new-point
                                   right-control)
                                  (make-point (/ (- x startx) 2)
                                              (/ (- y starty) 2)))))))))))))))))

;;; com-split-bezier-curve-segment
(define-clim-paint-command (com-split-bezier-curve-segment)
    ((presentation t))
  (let ((bezier-curve-segment (presentation-object presentation)))
    (com-drag-split-bezier-curve-segment bezier-curve-segment presentation *application-frame*))
  (let* ((frame *application-frame*)
         (app-pane (find-pane-named frame 'app)))
    (setf (pane-needs-redisplay app-pane) t)
    (clim:redisplay-frame-pane *application-frame* app-pane)))

(define-gesture-name split-bezier-curve-segment-gesture :pointer-button (:right))

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

;;; com-add-bezier-curve
(define-clim-paint-command (com-add-bezier-curve :name t)
    (&key
     (ink color))
  (with-accessors ((shapes shapes)
                   (default-ink ink))
      *application-frame*
    (let ((bezier-curve (make-paint-bezier-curve (coord-seq-to-point-seq
                                  (list 20 250 20 180 90 210 90 270 90 320 140 310 140 240))
                                 :ink (or ink default-ink)
                                 :line-thickness 4)))
      (push bezier-curve shapes))))

(defun bezier-point-update-callback (button)
  (declare (ignore button))
  (let ((properties-pane (find-pane-named *application-frame* 'properties)))
    (let ((object (pane-object properties-pane)))
      (declare (ignore object))
      (let ((x (parse-number:parse-number
                (gadget-value (find-pane-named *application-frame* 'x-pos))))
            (y (parse-number:parse-number
                (gadget-value (find-pane-named *application-frame* 'y-pos)))))
        (declare (ignore x y))
        ;; FIXME!!
        )))
  (let* ((frame *application-frame*)
         (app-pane (find-pane-named frame 'app)))
    (setf (pane-needs-redisplay app-pane) t)
    (clim:redisplay-frame-pane *application-frame* app-pane)))

(defmethod make-properties-pane ((curve paint-bezier-curve))
  (multiple-value-bind (x y)
      ;; FIXME
      (values 0 0)
    (make-pane
     'properties-pane
     :name 'properties
     :object curve
     :contents
     (list
      (labelling (:label "Bezier Point Properties")
        (vertically ()
          (horizontally ()
            (labelling (:label "X Position")
              (make-pane 'text-field
                         :name 'x-pos
                         :editable-p t
                         :value (princ-to-string x)))
            (labelling (:label "Y Position")
              (make-pane 'text-field
                         :name 'y-pos
                         :editable-p t
                         :value (princ-to-string y))))
          (make-pane 'push-button
                     :name 'bezier-point-update
                     :label "Update (FIXME!)"
                     :activate-callback 'bezier-point-update-callback
                     :max-height 20)))))))

 
