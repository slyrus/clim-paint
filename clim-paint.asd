;;;; clim-paint.asd

(asdf:defsystem #:clim-paint
  :description "A McCLIM Paiting Program"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :license  "BSD"
  :version "0.0.1"
  :depends-on (:mcclim)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "geometry")
               (:file "presentation")
               (:file "frame")
               (:file "paint-object")
               (:file "selection")
               (:file "point")
               (:file "line")
               (:file "rectangle")
               (:file "ellipse")
               (:file "bezier-curve")
               (:file "clim-paint")))
