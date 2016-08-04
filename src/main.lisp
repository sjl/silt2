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


(defun write-string-at (string x y)
  (charms:write-string-at-point
    charms:*standard-window*
    string
    (clamp-w x)
    (clamp-h y)))


(defun write-centered (text x y)
  (etypecase text
    (string (write-centered (list text) x y))
    (list (iterate
            (for string :in text)
            (for tx = (- x (floor (length string) 2)))
            (for ty :from y)
            (write-string-at string tx ty)))))

(defun write-left (text x y)
  (etypecase text
    (string (write-left (list text) x y))
    (list (iterate
            (for string :in text)
            (for tx = x)
            (for ty :from y)
            (write-string-at string tx ty)))))


(defun render-title ()
  (render
    (let ((cx (floor *width* 2))
          (cy (floor *height* 2)))
      (write-centered '("S I L T"
                        ""
                        "Press any key to start...")
                      cx (1- cy)))))

(defun render-intro ()
  (render
    (write-left '("Welcome to Silt."
                  ""
                  "You are the god of a toroidal world.")
                0 0)))


(defun handle-input-title ()
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))

(defun handle-input-intro ()
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))


(defun state-title ()
  (render-title)
  (handle-input-title)
  (state-intro))

(defun state-intro ()
  (render-intro)
  (handle-input-intro)
  (state-quit))

(defun state-quit ()
  'goodbye)


(defun run ()
  (setf *running* t)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    ; (charms:enable-non-blocking-mode charms:*standard-window*)
    (state-title)))

; (run)
