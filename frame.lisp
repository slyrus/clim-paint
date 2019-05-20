
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
        :min-height 600 :min-width 900
        :display-function 'clim-paint-display
        :default-view (make-instance 'clim-paint-view))
   (interactor :interactor :height 300 :width 900)

   ;; ellipse
   #|
   (ellipse-center-x text-field :editable-p t :value "0")
   (ellipse-center-y text-field :editable-p t :value "0")
   (ellipse-radius-1-dx text-field :editable-p t :value "0")
   (ellipse-radius-1-dy text-field :editable-p t :value "0")
   (ellipse-radius-2-dx text-field :editable-p t :value "0")
   (ellipse-radius-2-dy text-field :editable-p t :value "0")
   (ellipse-start-angle text-field :editable-p t :value "0")
   (ellipse-end-angle text-field :editable-p t :value "0")
   (ellipse-update :push-button :label "Update" :activate-callback 'ellipse-update-callback
                   :max-height 20)
   |#
   
   (properties (make-pane 'properties-pane :name 'properties)))

  (:layouts
   (default
       (vertically ()
         (horizontally ()
           (4/5 app)
           (1/5 properties))
         interactor))

   #+nil
   (ellipse
    (vertically ()
      (horizontally ()
        (4/5 app)
        (1/5 (labelling (:label "Properties")
               (vertically ()
                 (horizontally ()
                   (labelling (:label "Center X")
                     ellipse-center-x)
                   (labelling (:label "Center Y")
                     ellipse-center-y))
                 (horizontally ()
                   (labelling (:label "Radius 1 DX")
                     ellipse-radius-1-dx)
                   (labelling (:label "Radius 1 DY")
                     ellipse-radius-1-dy))
                 (horizontally ()
                   (labelling (:label "Radius 2 DX")
                     ellipse-radius-2-dx)
                   (labelling (:label "Radius 2 DY")
                     ellipse-radius-2-dy))
                 (horizontally ()
                   (labelling (:label "Start Angle")
                     ellipse-start-angle)
                   (labelling (:label "End Angle")
                     ellipse-end-angle))
                 ellipse-update
                 properties))))
      interactor))
   ))

