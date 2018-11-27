
(in-package :clim-paint)

;;
;; clim-paint view
(defclass clim-paint-view (view) ())

;;;
;;; clim-paint frame
(define-application-frame clim-paint ()
  ((shapes :initform nil :initarg :shapes :accessor shapes)
   (ink :initform +blue+ :accessor ink))
  (:menu-bar clim-paint-menubar)
  (:panes
   (app :application
        :height 600 :width 800
        :display-function #'clim-paint-display
        :default-view (make-instance 'clim-paint-view))
   (interactor :interactor :height 300 :width 800))
  (:layouts
   (default
       (vertically ()
         app
         interactor))))

