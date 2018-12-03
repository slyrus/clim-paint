
(in-package :clim-paint)

;;
;; clim-paint view
(defclass clim-paint-view (view) ())

;;;
;;; clim-paint frame
(define-application-frame clim-paint ()
  ((shapes :initform nil :initarg :shapes :accessor shapes)
   (selected-object-hash :initform (make-hash-table) :accessor selected-object-hash)
   (ink :initform +blue+ :accessor ink))
  (:menu-bar clim-paint-menubar)
  (:panes
   (app :application
        :background *background-color*
        :height 600 :width 800
        :display-function 'clim-paint-display
        :default-view (make-instance 'clim-paint-view))
   (interactor :interactor :height 300 :width 900)
   ;; point
   (point-x-pos text-field :editable-p t :value "0")
   (point-y-pos text-field :editable-p t :value "0")
   (point-update :push-button :label "Update" :activate-callback 'point-update-callback
                 :max-height 20)
   ;; line
   (line-x1-pos text-field :editable-p t :value "0")
   (line-y1-pos text-field :editable-p t :value "0")
   (line-x2-pos text-field :editable-p t :value "0")
   (line-y2-pos text-field :editable-p t :value "0")
   (line-update :push-button :label "Update" :activate-callback 'line-update-callback
                :max-height 20)
   ;; rectangle
   (rectangle-x1-pos text-field :editable-p t :value "0")
   (rectangle-y1-pos text-field :editable-p t :value "0")
   (rectangle-x2-pos text-field :editable-p t :value "0")
   (rectangle-y2-pos text-field :editable-p t :value "0")
   (rectangle-update :push-button :label "Update" :activate-callback 'rectangle-update-callback
                     :max-height 20)
   ;; ellipse
   (ellipse-x-pos text-field :editable-p t :value "0")
   (ellipse-y-pos text-field :editable-p t :value "0")
   (ellipse-update :push-button :label "Update" :activate-callback 'ellipse-update-callback
                   :max-height 20)
   ;; bezier-curve
   (bezier-curve-update :push-button :label "Update" :activate-callback 'bezier-curve-update-callback
                        :max-height 20)
   (properties (make-pane 'properties-pane :name 'properties)))

  ;; this commented out line is a reminder that there are two bugs in
  ;; the CLIM spec. It should be make-pane not make-isntance and the
  ;; argument should be :label not :text.
  ;;
  ;; (l1 (make-pane 'label-pane :label "One"))

  (:layouts
   (default
       (vertically ()
         (horizontally ()
           (4/5 app)
           (1/5 (labelling (:label "Properties"))))
         interactor))

   (point
    (vertically ()
      (horizontally ()
        (4/5 app)
        (1/5 (labelling (:label "Properties")
               (vertically ()
                 (horizontally ()
                   (labelling (:label "X Position")
                     point-x-pos)
                   (labelling (:label "Y Position")
                     point-y-pos))
                 point-update
                 properties))))
      interactor))

   (line
    (vertically ()
      (horizontally ()
        (4/5 app)
        (1/5 (labelling (:label "Properties")
               (vertically ()
                 (horizontally ()
                   (labelling (:label "X1 Position")
                     line-x1-pos)
                   (labelling (:label "Y1 Position")
                     line-y1-pos))
                 (horizontally ()
                   (labelling (:label "X2 Position")
                     line-x2-pos)
                   (labelling (:label "Y2 Position")
                     line-y2-pos))
                 line-update
                 properties))))
      interactor))

   (rectangle
    (vertically ()
      (horizontally ()
        (4/5 app)
        (1/5 (labelling (:label "Properties")
               (vertically ()
                 (horizontally ()
                   (labelling (:label "X1 Position")
                     rectangle-x1-pos)
                   (labelling (:label "Y1 Position")
                     rectangle-y1-pos))
                 (horizontally ()
                   (labelling (:label "X2 Position")
                     rectangle-x2-pos)
                   (labelling (:label "Y2 Position")
                     rectangle-y2-pos))
                 rectangle-update
                 properties))))
      interactor))

   (ellipse
    (vertically ()
      (horizontally ()
        (4/5 app)
        (1/5 (labelling (:label "Properties")
               (vertically ()
                 (horizontally ()
                   (labelling (:label "X Position")
                     ellipse-x-pos)
                   (labelling (:label "Y Position")
                     ellipse-y-pos))
                 ellipse-update
                 properties))))
      interactor))

   (bezier-curve
    (vertically ()
      (horizontally ()
        (4/5 app)
        (1/5 (labelling (:label "Properties")
               (vertically ()
                 bezier-curve-update
                 properties))))
      interactor))))

