(in-package #:silt)

;;;; Data
(defparameter *running* nil)
(defparameter *running* t)
(defparameter *debug* nil)

(defparameter *screen-width* 1)
(defparameter *screen-height* 1)
(defparameter *screen-center-x* 1)
(defparameter *screen-center-y* 1)

(defparameter *world-exponent* 9)
(defparameter *world-size* (expt 2 *world-exponent*))

(defparameter *view-x* 0)
(defparameter *view-y* 0)

(defparameter *cursor-x* 0)
(defparameter *cursor-y* 0)

(defvar *heightmap* nil)


;;;; Colors
(define-constant +color-white+ 0)
(define-constant +color-blue+ 1)
(define-constant +color-yellow+ 2)
(define-constant +color-cyan+ 3)
(define-constant +color-snow+ 4)
(define-constant +color-green+ 5)
(define-constant +color-pink+ 6)

(defun init-colors ()
  (charms/ll:init-pair +color-white+ charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-blue+ charms/ll:COLOR_BLUE charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-yellow+ charms/ll:COLOR_YELLOW charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-cyan+ charms/ll:COLOR_CYAN charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-snow+ charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)
  (charms/ll:init-pair +color-green+ charms/ll:COLOR_GREEN charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-pink+ charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK))

(defmacro with-color (color &body body)
  (once-only (color)
    `(unwind-protect
      (prog2
        (charms/ll:attron (charms/ll:color-pair ,color))
        (progn ,@body))
      (charms/ll:attroff (charms/ll:color-pair ,color)))))


;;;; Utils
(deftype world-coordinate ()
  `(integer 0 ,(1- array-dimension-limit)))

(deftype world-array ()
  `(simple-array single-float (* *)))


(defun manage-screen ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (setf *screen-width* (1- w) *screen-height* (1- h)
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

(defun write-right (text x y)
  (etypecase text
    (string (write-right (list text) x y))
    (list (iterate
            (for string :in text)
            (for tx = (- x (length string)))
            (for ty :from y)
            (write-string-at string tx ty)))))


(defun l (s &rest args)
  (write-centered (apply #'format nil s args)
                  *screen-center-x* *screen-center-y*))


(defclause-sequence ACROSS-FLAT-ARRAY INDEX-OF-FLAT-ARRAY
  :access-fn 'row-major-aref
  :size-fn 'array-total-size
  :sequence-type 'array
  :element-type t)


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
        (spread-reduction 0.6))
    (recursively ((radius (floor (hm-size heightmap) 2))
                  (spread spread))
      (when (>= radius 1)
        (ds-squares heightmap radius spread)
        (ds-diamonds heightmap radius spread)
        (recur (/ radius 2)
               (* spread spread-reduction)))))
  (normalize-heightmap heightmap)
  heightmap)


;;;; Miscellaneous
(defun move-view (dx dy)
  (setf *view-x* (wrap (+ *view-x* dx))
        *view-y* (wrap (+ *view-y* dy))))

(defun move-cursor (dx dy)
  (setf *cursor-x* (clamp-w (+ *cursor-x* dx))
        *cursor-y* (clamp-h (+ *cursor-y* dy))))

(defun wrap (coord)
  (mod coord *world-size*))

(defun terrain-type (x y)
  (let ((h (aref *heightmap* (wrap x) (wrap y))))
    (cond ((< h 0.23)  :deep-water)
          ((< h 0.3)  :shallow-water)
          ((< h 0.34) :sand)
          ((< h 0.65) :grass)
          ((< h 0.7)  :dirt)
          ((< h 0.75) :hills)
          ((< h 0.9)  :mountain)
          (t          :snow))))

(defun terrain-char (x y)
  (case (terrain-type x y)
    (:deep-water    (values #\~ +color-blue+))
    (:shallow-water (values #\~ +color-cyan+))
    (:sand          (values #\: +color-yellow+))
    (:grass         (values #\. +color-green+))
    (:dirt          (values #\. +color-white+))
    (:hills         (values #\^ +color-white+))
    (:mountain      (values #\# +color-white+))
    (:snow          (values #\* +color-snow+))))

(defun world-to-screen (wx wy)
  "Convert world-space coordinates to screen-space."
  (values (wrap (- wx *view-x*))
          (wrap (- wy *view-y*))))

(defun screen-to-world (sx sy)
  "Convert screen-space coordinates to world-space."
  (values (wrap (+ sx *view-x*))
          (wrap (+ sy *view-y*))))

(defun onscreenp (sx sy)
  "Return whether the given screen-space coords are visible in the viewport."
  (and (< -1 sx *screen-width*)
       (< -1 sy *screen-height*)))


;;;; Roll-Your-Own-ECS
;;; Entities are stored in an {id -> entity} hash table.
;;;
;;; Entities are also indexed by component in a nested hash table:
;;;
;;;     {component-symbol -> {id -> entity}}
;;;
;;; Systems are stored as:
;;;
;;;     {system-symbol -> (cons system-function type-specifier-list)}
;;;
;;; TODO: Figure out the distinct problem.

(defvar *entity-id-counter* 0)
(defvar *entity-index* (make-hash-table))
(defvar *component-index* (make-hash-table))
(defvar *system-index* (make-hash-table))


(defun clear-entities ()
  (let ((ents (hash-table-values *entity-index*)))
    (clrhash *entity-index*)
    (mapc #'clrhash (hash-table-values *component-index*))
    (mapc #'entity-destroyed ents)))

(defun get-entity (id)
  (gethash id *entity-index*))


(defclass entity ()
  ((id :reader entity-id :initform (incf *entity-id-counter*))))

(defmethod print-object ((e entity) stream)
  (print-unreadable-object (e stream :type t :identity nil)
    (format stream "~D" (entity-id e))))

(defmethod initialize-instance :after ((e entity) &key)
  (setf (gethash (entity-id e) *entity-index*) e))


(defgeneric entity-created (entity)
  (:method ((entity entity)) nil))

(defgeneric entity-destroyed (entity)
  (:method ((entity entity)) nil))


(defun create-entity (class &rest initargs)
  (let ((entity (apply #'make-instance class initargs)))
    (entity-created entity)
    entity))

(defun destroy-entity (entity)
  (let ((id (entity-id entity)))
    (remhash id *entity-index*)
    (iterate
      (for (nil index) :in-hashtable *component-index*)
      (remhash id index)))
  (entity-destroyed entity)
  nil)


(defmacro define-entity (name components &rest slots)
  `(defclass ,name (entity ,@components)
     (,@slots)))


(defun initialize-component-index (name)
  (unless (hash-table-key-exists-p *component-index* name)
    (setf (gethash name *component-index*)
          (make-hash-table))))

(defmacro define-component (name &rest fields)
  (flet ((clean-field (f)
           (etypecase f
             (symbol (list f))
             (list f))))
    `(progn
      (defclass ,name ()
        ,(iterate
           (for (field . field-options) :in (mapcar #'clean-field fields))
           (for field-name = (symbolize name '/ field))
           (collect `(,field-name
                      :accessor ,field-name
                      :initarg ,(intern (symbol-name field-name) "KEYWORD")
                      ,@field-options))))

      (initialize-component-index ',name)

      (defmethod initialize-instance :after ((o ,name) &key)
        (setf (gethash (entity-id o)
                       (gethash ',name *component-index*))
              o))

      (find-class ',name))))


(defmacro define-system (name arglist &body body)
  `(prog1
    (declaim (ftype (function
                      (,@(mapcar (lambda (arg)
                                   `(and entity ,@(cdr arg)))
                                 arglist))
                      (values null &optional))
                    ,name))
    (defun ,name (,@(mapcar #'car arglist))
      ,@body
      nil)
    (setf (gethash ',name *system-index*) (cons #',name ',(mapcar #'cdr arglist)))))

(defun run-system (system)
  (flet ((retrieve-entities (specifier)
           (if (null specifier)
             (hash-table-values *entity-index*)
             (apply #'intersection
                    (mapcar (lambda (component)
                              (hash-table-values
                                (gethash component *component-index*)))
                            specifier)))))
    (destructuring-bind (system-function . type-specifiers)
        (gethash system *system-index*)
      (apply #'map-product system-function
             (mapcar #'retrieve-entities type-specifiers))
      (values))))


;;;; Coordinates
(define-component coords x y)


(defparameter *coords-contents* (make-hash-table))


(defun coordinate-key (x y)
  (array-row-major-index *heightmap* (wrap x) (wrap y)))

(defun coordinate-key-for-entity (e)
  (coordinate-key (coords/x e) (coords/y e)))


(defun coords-insert-entity (e)
  (push e (gethash (coordinate-key-for-entity e) *coords-contents*)))

(defun coords-remove-entity (e)
  (let ((k (coordinate-key-for-entity e)))
    (when (null (zap% (gethash k *coords-contents*)
                      #'delete e %))
      (remhash k *coords-contents*))))

(defun coords-move-entity (e new-x new-y)
  (coords-remove-entity e)
  (setf (coords/x e) new-x
        (coords/y e) new-y)
  (coords-insert-entity e))

(defun coords-lookup (x y)
  (gethash (coordinate-key x y) *coords-contents*))


(defmethod entity-created :after ((entity coords))
  (coords-insert-entity entity))

(defmethod entity-destroyed :after ((entity coords))
  (coords-remove-entity entity))


;;;; Flavor Text
(define-component flavor text)

;;; Components
(define-component visible
  (glyph :type string)
  color)

(define-component edible
  energy)

(define-component fruiting
  chance)


;;; Entities
(define-entity fruit (coords visible edible flavor))
(define-entity tree (coords visible fruiting flavor))
(define-entity algae (coords visible edible))

(defun make-tree (x y)
  (create-entity 'tree
                 :coords/x x
                 :coords/y y
                 :visible/glyph "T"
                 :visible/color +color-green+
                 :fruiting/chance 0.0001
                 :flavor/text "A tree sways gently in the wind."))

(defun make-fruit (x y)
  (create-entity 'fruit
                 :coords/x x
                 :coords/y y
                 :visible/glyph "ó"
                 :visible/color +color-pink+
                 :edible/energy (random-around 10 3)
                 :flavor/text "A ripe piece of fruit has fallen to the ground."))

(defun make-algae (x y)
  (create-entity 'algae
                 :coords/x x
                 :coords/y y
                 :visible/glyph "`"
                 :visible/color +color-green+))


;;; Systems

(define-system grow-fruit ((entity fruiting coords))
  (when (< (random 1.0) (fruiting/chance entity))
    (make-fruit (wrap (random-around (coords/x entity) 2))
                (wrap (random-around (coords/y entity) 2)))))


;;;; Flora
(defun tree-probability (x y)
  (case (terrain-type x y)
    (:grass 0.01)
    (:dirt 0.001)
    (t 0)))

(defun algae-probability (x y)
  (case (terrain-type x y)
    (:shallow-water 0.01)
    (:deep-water 0.001)
    (t 0)))

(defun grow-trees ()
  (iterate
    (for x :from 0 :below *world-size*)
    (iterate
      (for y :from 0 :below *world-size*)
      (when (< (random 1.0) (tree-probability x y))
        (make-tree x y)))))

(defun grow-algae ()
  (iterate
    (for x :from 0 :below *world-size*)
    (iterate
      (for y :from 0 :below *world-size*)
      (when (< (random 1.0) (algae-probability x y))
        (make-algae x y)))))


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
                  "You are the god of a toroidal world."
                  ""
                  "CONTROLS"
                  "  hjklyubn - move your view"
                  "  HJKLYUBN - move your view faster"
                  ""
                  "  wasd - move your cursor"
                  "  WASD - move your cursor faster"
                  ""
                  "  Q - quit"
                  "  R - regenerate the world"
                  ""
                  "  ? - help"
                  ""
                  "Press any key to begin."
                  )
                1 1)))

(defun render-help ()
  (render
    (write-left '("CONTROLS"
                  "  hjklyubn - move your view"
                  "  HJKLYUBN - move your view faster"
                  ""
                  "  wasd - move your cursor"
                  "  WASD - move your cursor faster"
                  ""
                  "  Q - quit"
                  "  R - regenerate the world"
                  ""
                  "  ? - help"
                  ""
                  "Press any key to continue."
                  )
                1 1)))

(defun render-generate ()
  (render
    (write-centered "Generating world, please wait..."
                    *screen-center-x* *screen-center-y*)))


(defun draw-terrain ()
  (iterate
    (repeat *screen-width*)
    (for sx :from 0)
    (for wx :from *view-x*)
    (iterate
      (repeat *screen-height*)
      (for sy :from 0)
      (for wy :from *view-y*)
      (for (values terrain-char terrain-color) = (terrain-char wx wy))
      (for contents = (remove-if-not (lambda (e) (typep e 'visible))
                                     (coords-lookup wx wy)))
      (if contents
        (with-color (visible/color (car contents))
          (charms:write-string-at-point
            charms:*standard-window*
            (visible/glyph (car contents))
            sx sy))
        (with-color terrain-color
          (charms:write-char-at-point
            charms:*standard-window*
            terrain-char
            sx sy))))))

(defun draw-ui ()
  (write-right
    (list
      (format nil "[~D, ~D]" *view-x* *view-y*)
      (format nil "[~D, ~D]" *cursor-x* *cursor-y*)
      (format nil "~D entities" (hash-table-count *entity-index*)))
    (1- *screen-width*)
    0)
  (write-left
    (iterate
      (for entity :in (multiple-value-call #'coords-lookup
                        (screen-to-world *cursor-x* *cursor-y*)))
      (when (typep entity 'flavor)
        (collect (flavor/text entity))))
    0 0))


(defun render-map ()
  (manage-screen)
  (draw-terrain)
  (draw-ui)
  (charms:move-cursor charms:*standard-window* *cursor-x* *cursor-y*))


(defun press-any-key ()
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (charms:get-char charms:*standard-window*))

(defun handle-input-map ()
  (iterate
    (for key = (charms:get-char charms:*standard-window* :ignore-error t))
    (while key)
    (case key
      ((#\Q) (return :quit))
      ((#\R) (return :regen))
      ((#\?) (return :help))

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

      ((#\w) (move-cursor  0 -1))
      ((#\a) (move-cursor -1  0))
      ((#\s) (move-cursor  0  1))
      ((#\d) (move-cursor  1  0))
      ((#\W) (move-cursor   0 -10))
      ((#\A) (move-cursor -10   0))
      ((#\S) (move-cursor   0  10))
      ((#\D) (move-cursor  10   0))

      (t (push key *debug*) t))))


(defun tick-world ()
  (run-system 'grow-fruit))


(defun state-title ()
  (render-title)
  (press-any-key)
  (state-intro))

(defun state-intro ()
  (render-intro)
  (press-any-key)
  (state-generate))

(defun state-generate ()
  (render-generate)
  (clear-entities)
  (setf *heightmap* (diamond-square (allocate-heightmap))
        *view-x* 0
        *view-y* 0
        *cursor-x* 0
        *cursor-y* 0)
  (grow-trees)
  (grow-algae)
  (state-map))

(defun state-map ()
  (charms:enable-non-blocking-mode charms:*standard-window*)
  (case (handle-input-map)
    ((:quit) (state-quit))
    ((:regen) (state-generate))
    ((:help) (state-help))
    (t
     (tick-world)
     (render-map)
     (sleep 0.02)
     (state-map))))


(defun state-help ()
  (render-help)
  (press-any-key)
  (state-map))

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
    (init-colors)
    (state-title)))

(defun main ()
  (handler-case
    (progn
      (run)
      (format t "Goodbye.~%"))
    (t (e)
     (declare (ignore e))
     (format t "Something went wrong, sorry.~%"))))

; (run)
