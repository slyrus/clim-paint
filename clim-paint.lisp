
(in-package :clim-paint)



;;;
;;; main display function
(defun clim-paint-display (frame pane)
  (declare (ignore pane))
  (with-accessors ((shapes shapes))
      frame
    (mapcar (lambda (x)
              (present x (class-of x) :single-box t))
            shapes)))

;;;
;;; menus, menu bars, and application commands
(define-clim-paint-command (com-quit :name t :menu "Quit")
    ()
  (frame-exit *application-frame*))

(define-clim-paint-command (com-export-to-pdf :name t :menu "Export to PDF")
    ((pdf-pathname pathname
                   :default *default-pathname-defaults* :insert-default t))
  (let* ((frame *application-frame*)
         (pane (find-pane-named frame 'app))
         (device-type :a4))
    (with-open-file (file-stream pdf-pathname :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (clim-pdf:with-output-to-pdf-stream
          (stream file-stream
                  :header-comments '(:title "clim-paint")
                  :scale-to-fit t
                  :device-type device-type)
        (setf (stream-default-view stream)
              (or (stream-default-view pane)
                  (make-instance 'clim-paint-view)))
        (let ((*standard-output* stream))
          (with-accessors ((shapes shapes))
              frame
            (mapcar (lambda (x)
                      (present x (class-of x) :single-box t))
                    shapes)))))))

(make-command-table 'clim-paint-file-command-table
                    :errorp nil
                    :menu '(("Quit" :command com-quit)))

(make-command-table 'clim-paint-menubar
                    :errorp nil
                    :menu '(("File" :menu clim-paint-file-command-table)))

;;;
;;; the actual application
(defvar *clim-paint-app*)

(defun coord-seq-to-point-seq (coord-seq)
  (loop for (x y) on coord-seq by #'cddr
     collect (make-point x y)))

(defun make-default-shapes ()
  (list (make-paint-point 10 20 :ink +red+)
        (make-paint-point 30 20 :ink +green+)
        (make-paint-point 50 20 :ink +blue+)

        (make-paint-rectangle 50 160 100 200 :ink +blue+ :filled t)
        (make-paint-rectangle 350 210 400 250 :ink +dark-grey+ :filled t)

        (make-paint-bezier-curve (coord-seq-to-point-seq
                                  (list 20 150 20 80 90 110 90 170 90 220 140 210 140 140))
                                 :ink +light-blue+
                                 :line-thickness 4)

        (make-paint-ellipse (make-point 100 100)
                            10 30 40 15
                            :ink +orange+
                            :filled t)
        (make-paint-ellipse (make-point 250 100)
                            30 0 0 40
                            :ink +brown+
                            :filled t)
        (make-paint-ellipse (make-point 225 200)
                            150 -90 60 25
                            :ink +green+
                            :line-thickness 5
                            :filled nil)
        ))

(defun clim-paint (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'clim-paint :shapes (make-default-shapes))))
             (setf *clim-paint-app* frame)
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "clim-paint")
        (run))))

