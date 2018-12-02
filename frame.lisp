
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
   (properties (clim:make-pane 'properties-pane
                               :name 'properties
                               :height 600 :width 300
                               :display-function 'properties-display
                               :default-view (make-instance 'properties-view)))
   (interactor :interactor :height 300 :width 900))
  (:layouts
   (default
       (vertically ()
         (horizontally ()
           (4/5 app)
           (1/5 (clim:labelling (:label "Properties")
                  properties)))
         interactor))))

