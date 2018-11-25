
(in-package :clim-paint)

(defclass paint-object ()
  ((ink :initarg :ink :accessor ink)
   (filled :initarg :filled :accessor filledp)))

;;;
;;; some special variables to be used for drawing/dragging
(defparameter *highlight-color* +orange+)

(defparameter *selection-color* +red+)

(defparameter *drag-color* +green+)
