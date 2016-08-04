(in-package #:silt)

;;;; Data
(defparameter *running* nil)
(defparameter *running* t)
(defparameter *debug* nil)

(defparameter *screen-width* 1)
(defparameter *screen-height* 1)
(defparameter *screen-center-x* 1)
(defparameter *screen-center-y* 1)

(defparameter *world-exponent* 10)
(defparameter *world-size* (expt 2 *world-exponent*))

(defparameter *view-x* 0)
(defparameter *view-y* 0)

(defvar *heightmap* nil)


;;;; Colors
(define-constant +color-white+ 0)
(define-constant +color-blue+ 1)
(define-constant +color-yellow+ 2)
(define-constant +color-cyan+ 3)
(define-constant +color-snow+ 4)
(define-constant +color-green+ 5)

(charms/ll:init-pair +color-white+ charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)
(charms/ll:init-pair +color-blue+ charms/ll:COLOR_BLUE charms/ll:COLOR_BLACK)
(charms/ll:init-pair +color-yellow+ charms/ll:COLOR_YELLOW charms/ll:COLOR_BLACK)
(charms/ll:init-pair +color-cyan+ charms/ll:COLOR_CYAN charms/ll:COLOR_BLACK)
(charms/ll:init-pair +color-snow+ charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)
(charms/ll:init-pair +color-green+ charms/ll:COLOR_GREEN charms/ll:COLOR_BLACK)

(defmacro with-color (color &body body)
  (once-only (color)
    `(prog2
      (charms/ll:attron (charms/ll:color-pair ,color))
      (progn ,@body)
      (charms/ll:attroff (charms/ll:color-pair ,color)))))


;;;; Utils
(defun manage-screen ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (setf *screen-width* w *screen-height* h
          *screen-center-x* (floor w 2)
          *screen-center-y* (floor h 2))))


(defmacro render (&body body)
  `(prog2
    (progn
      (manage-screen)
      (charms:clear-window charms:*standard-window*))
    (progn ,@body)
    (charms:refresh-window charms:*standard-window*)))

(defun clamp-w (x)
  (clamp 0 (1- *screen-width*) x))

(defun clamp-h (y)
  (clamp 0 (1- *screen-height*) y))


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


;;;; World Generation
(defun jitter (value spread)
  (+ value (- (random (* 2.0 spread))
              spread)))

(defun average (&rest values)
  (/ (apply #'+ values) (length values)))


(defun allocate-heightmap ()
  (make-array (list *world-size* *world-size*)
    :element-type 'single-float
    :initial-element 0.0
    :adjustable nil))


(defun hm-size (heightmap)
  (first (array-dimensions heightmap)))

(defun hm-ref (heightmap x y)
  (let ((last (hm-size heightmap)))
    (aref heightmap
          (cond
            ((< -1 x last) x)
            ((= x last) 0)
            (t (mod x last)))
          (cond
            ((< -1 y last) y)
            ((= y last) 0)
            (t (mod y last))))))


(defun normalize-heightmap (heightmap)
  (iterate
    (for i :from 0 :below (array-total-size heightmap))
    (for v = (row-major-aref heightmap i))
    (maximize v :into max)
    (minimize v :into min)
    (finally
      (iterate
        (with span = (- max min))
        (for i :from 0 :below (array-total-size heightmap))
        (for v = (row-major-aref heightmap i))
        (setf (row-major-aref heightmap i)
              (/ (- v min) span))))))


(defun ds-init (heightmap)
  (setf (aref heightmap 0 0) 0.5))


(defun ds-square (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (jitter (average (hm-ref heightmap (- x radius) (- y radius))
                         (hm-ref heightmap (- x radius) (+ y radius))
                         (hm-ref heightmap (+ x radius) (- y radius))
                         (hm-ref heightmap (+ x radius) (+ y radius)))
                spread)))

(defun ds-diamond (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (jitter (average (hm-ref heightmap (- x radius) y)
                         (hm-ref heightmap (+ x radius) y)
                         (hm-ref heightmap x (- y radius))
                         (hm-ref heightmap x (+ y radius)))
                spread)))


(defun ds-squares (heightmap radius spread)
  (iterate
    (for x :from radius :below (hm-size heightmap) :by (* 2 radius))
    (iterate
      (for y :from radius :below (hm-size heightmap) :by (* 2 radius))
      (ds-square heightmap x y radius spread))))

(defun ds-diamonds (heightmap radius spread)
  (iterate
    (for i :from 0)
    (for y :from 0 :below (hm-size heightmap) :by radius)
    (for shift = (if (evenp i) radius 0))
    (iterate
      (for x :from shift :below (hm-size heightmap) :by (* 2 radius))
      (ds-diamond heightmap x y radius spread))))


(defun diamond-square (heightmap)
  (ds-init heightmap)
  (let ((spread 0.7)
        (spread-reduction 0.5))
    (recursively ((radius (floor (hm-size heightmap) 2))
                  (spread spread))
      (when (>= radius 1)
        (ds-squares heightmap radius spread)
        (ds-diamonds heightmap radius spread)
        (recur (/ radius 2)
               (* spread spread-reduction)))))
  (normalize-heightmap heightmap)
  heightmap)


;;;;
(defun move-view (dx dy)
  (incf *view-x* dx)
  (incf *view-y* dy))

(defun wrap (coord)
  (mod coord *world-size*))

(defun terrain-char (x y)
  (let ((h (aref *heightmap* (wrap x) (wrap y))))
    (cond ((< h 0.2)  (values #\~ +color-blue+))
          ((< h 0.3)  (values #\~ +color-cyan+))
          ((< h 0.32) (values #\_ +color-yellow+))
          ((< h 0.65) (values #\. +color-green+))
          ((< h 0.7)  (values #\. +color-white+))
          ((< h 0.75) (values #\^ +color-white+))
          ((< h 0.9)  (values #\# +color-white+))
          (t          (values #\* +color-snow+)))))


;;;; Game State Machine
(defun render-title ()
  (render
    (write-centered '("S I L T"
                      ""
                      "Press any key to start...")
                    *screen-center-x*
                    (1- *screen-center-y*))))

(defun render-intro ()
  (render
    (write-left '("Welcome to Silt."
                  ""
                  "You are the god of a toroidal world.")
                0 0)))

(defun render-generate ()
  (render
    (write-centered "Generating world, please wait..."
                    *screen-center-x* *screen-center-y*)))

(defun render-map ()
  (iterate
    (repeat *screen-width*)
    (for sx :from 0)
    (for wx :from *view-x*)
    (iterate
      (repeat *screen-height*)
      (for sy :from 0)
      (for wy :from *view-y*)
      (for (values char color) = (terrain-char wx wy))
      (with-color color
        (charms:write-char-at-point
          charms:*standard-window*
          char
          sx sy)))))


(defun handle-input-title ()
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))

(defun handle-input-intro ()
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))

(defun handle-input-map ()
  (iterate
    (for key = (charms:get-char charms:*standard-window* :ignore-error t))
    (while key)
    (case key
      ((#\Q) (return :quit))
      ((#\R) (return :regen))

      ((#\h) (move-view  -5   0))
      ((#\j) (move-view   0   5))
      ((#\k) (move-view   0  -5))
      ((#\l) (move-view   5   0))
      ((#\y) (move-view  -5  -5))
      ((#\u) (move-view   5  -5))
      ((#\b) (move-view  -5   5))
      ((#\n) (move-view   5   5))

      ((#\H) (move-view -30   0))
      ((#\J) (move-view   0  30))
      ((#\K) (move-view   0 -30))
      ((#\L) (move-view  30   0))
      ((#\Y) (move-view -30 -30))
      ((#\U) (move-view  30 -30))
      ((#\B) (move-view -30  30))
      ((#\N) (move-view  30  30))

      (t (push key *debug*) t))))


(defun state-title ()
  (render-title)
  (handle-input-title)
  (state-intro))

(defun state-intro ()
  (render-intro)
  (handle-input-intro)
  (state-generate))

(defun state-generate ()
  (render-generate)
  (setf *heightmap* (diamond-square (allocate-heightmap)))
  (state-map))

(defun state-map ()
  (charms:enable-non-blocking-mode charms:*standard-window*)
  (case (handle-input-map)
    ((:quit)
     (state-quit))
    ((:regen)
     (state-generate))
    (t
     (render-map)
     (sleep 0.1)
     (state-map))))

(defun state-quit ()
  'goodbye)


;;;; Run
(defun run ()
  (setf *running* t)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys charms:*standard-window*)
    (charms/ll:start-color)
    (state-title)))

; (run)
