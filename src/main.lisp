(in-package #:silt)

(defparameter *running* nil)
(defparameter *running* t)

(defparameter *width* 1)
(defparameter *height* 1)


(defun manage-screen ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (setf *width* w *height* h)))


(defmacro render (&body body)
  `(prog2
    (progn
      (manage-screen)
      (charms:clear-window charms:*standard-window*))
    (progn ,@body)
    (charms:refresh-window charms:*standard-window*)))

(defun clamp-w (x)
  (clamp 0 (1- *width*) x))

(defun clamp-h (y)
  (clamp 0 (1- *height*) y))

(defun write-centered (string x y)
  (charms:write-string-at-point
    charms:*standard-window*
    string
    (clamp-w (- x (floor (length string) 2)))
    (clamp-h y)))

(defun write-left (string x y)
  (charms:write-string-at-point
    charms:*standard-window*
    string
    (clamp-w x)
    (clamp-h y)))


(defun render-title ()
  (render
    (let ((cx (floor *width* 2))
          (cy (floor *height* 2)))
      (write-centered "S I L T" cx cy) 
      (write-centered "Press any key to start..." cx (1+ cy)) )))

(defun render-intro ()
  (render
    (charms:move-cursor charms:*standard-window*
                        (- (floor *width* 2) 3)
                        (floor *height* 2))
    (write-left "Welcome to Silt." 0 0)
    (write-left "You are the god of a toroidal world." 0 1)))


(defun handle-input-title ()
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))

(defun handle-input-intro ()
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))


(defparameter *game*
  (state-machine ()
      ((title ()
         (render-title)
         (handle-input-title)
         (transition intro))
       (intro ()
         (render-intro)
         (handle-input-intro)
         (transition quit))
       (quit ()
         'goodbye))
    (transition title)))


(defun run ()
  (setf *running* t)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    ; (charms:enable-non-blocking-mode charms:*standard-window*)
    (invoke-state-machine *game*)))

; (run)
