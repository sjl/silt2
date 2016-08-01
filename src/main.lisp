(in-package #:silt)


(defparameter *running* nil)
(defparameter *running* t)

(defparameter *width* 1)
(defparameter *height* 1)

(defun render ()
  (charms:move-cursor charms:*standard-window*
                      (- (floor *width* 2) 3)
                      (floor *height* 2))
  (charms:write-string-at-cursor charms:*standard-window* "S I L T")
  (charms:move-cursor charms:*standard-window* 0 0))


(defun tick ()
  )

(defun handle-input ()
  (let ((input (charms:get-char charms:*standard-window* :ignore-error t)))
    (case input
      ((nil) nil)
      (#\q (setf *running* nil)))))

(defun manage-screen ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (setf *width* w *height* h)))

(defun run ()
  (setf *running* t)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (iterate
      (while *running*)
      (charms:clear-window charms:*standard-window*)
      (manage-screen)
      (handle-input)
      (tick)
      (render)
      (charms:refresh-window charms:*standard-window*)
      (sleep 0.03))))


; (run)
