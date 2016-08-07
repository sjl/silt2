(in-package #:silt)
(require :sb-sprof)

;;;; Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-constant +world-exponent+ 9)
(define-constant +world-size+ (expt 2 +world-exponent+))
(defparameter *screen-width* 1)
(defparameter *screen-height* 1)
(defparameter *screen-center-x* 1)
(defparameter *screen-center-y* 1)
(defparameter *view-x* 0)
(defparameter *view-y* 0)
(defparameter *cursor-x* 0)
(defparameter *cursor-y* 0)
(defparameter *paused* nil)
(defparameter *game-log* nil)
(defparameter *population* 0)
(defparameter *tick* 0)
(defparameter *timing* (cons 0 0))


(deftype world-coordinate ()
  `(integer 0 ,(1- +world-size+)))

(deftype world-array ()
  `(simple-array single-float (,+world-size+ ,+world-size+)))


(defun allocate-heightmap ()
  (make-array (list +world-size+ +world-size+)
    :element-type 'single-float
    :initial-element 0.0
    :adjustable nil))


(declaim (type world-array *heightmap*))
(defvar *heightmap* (allocate-heightmap))


;;;; Colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-constant +color-white+ 0)
(define-constant +color-blue+ 1)
(define-constant +color-yellow+ 2)
(define-constant +color-cyan+ 3)
(define-constant +color-snow+ 4)
(define-constant +color-green+ 5)
(define-constant +color-pink+ 6)
(define-constant +color-orange+ 7)

(defun init-colors ()
  (charms/ll:init-pair +color-white+ charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-blue+ charms/ll:COLOR_BLUE charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-yellow+ charms/ll:COLOR_YELLOW charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-cyan+ charms/ll:COLOR_CYAN charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-snow+ charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)
  (charms/ll:init-pair +color-green+ charms/ll:COLOR_GREEN charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-pink+ charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +color-orange+ charms/ll:COLOR_BLACK charms/ll:COLOR_YELLOW))

(defmacro with-color (color &body body)
  (once-only (color)
    `(unwind-protect
      (prog2
        (charms/ll:attron (charms/ll:color-pair ,color))
        (progn ,@body))
      (charms/ll:attroff (charms/ll:color-pair ,color)))))


;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun write-char-at (char x y)
  (charms:write-char-at-point
    charms:*standard-window*
    char
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


(defun log-message (s &rest args)
  (push (cons 200 (apply #'format nil s args)) *game-log*))


(defclause-sequence ACROSS-FLAT-ARRAY INDEX-OF-FLAT-ARRAY
  :access-fn 'row-major-aref
  :size-fn 'array-total-size
  :sequence-type 'array
  :element-type t)


(declaim (inline wrap)
         (ftype (function (fixnum) world-coordinate) wrap)
         (ftype (function (fixnum fixnum)) terrain-type terrain-char))

(defun wrap (coord)
  (mod coord +world-size+))

(defun move-view (dx dy)
  (setf *view-x* (wrap (+ *view-x* dx))
        *view-y* (wrap (+ *view-y* dy))))

(defun move-cursor (dx dy)
  (setf *cursor-x* (clamp-w (+ *cursor-x* dx))
        *cursor-y* (clamp-h (+ *cursor-y* dy))))


(defun terrain-type (x y)
  (let ((h (aref *heightmap* (wrap x) (wrap y))))
    (cond ((< h 0.23) :deep-water)
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


(defmacro timing (&body body)
  `(let ((start (get-internal-run-time)))
     (prog1
       (progn ,@body)
       (setf (cdr *timing*)
             (/ (+ (* (car *timing*) (cdr *timing*))
                   (- (get-internal-run-time) start))
               (incf (car *timing*)))))))

(defun reset-timing ()
  (setf *timing* (cons 0 0)))



;;;; Terrain Generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun average4 (a b c d)
  (/ (+ a b c d) 4))


(defun hm-ref (heightmap x y)
  (flet ((ref (n)
           (cond
             ((< -1 n +world-size+) n)
             ((= n +world-size+) 0)
             (t (mod n +world-size+)))))
    (aref heightmap (ref x) (ref y))))


(defun heightmap-extrema (heightmap)
  (iterate
    (for v :across-flat-array heightmap :with-index i)
    (maximize v :into max)
    (minimize v :into min)
    (finally (return (values min max)))))

(defun normalize-heightmap (heightmap)
  (multiple-value-bind (min max) (heightmap-extrema heightmap)
    (iterate
      (with span = (- max min))
      (for v :across-flat-array heightmap :with-index i)
      (setf (row-major-aref heightmap i)
            (/ (- v min) span)))))


(defun ds-init (heightmap)
  (setf (aref heightmap 0 0) 0.5))


(defun ds-square (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average4 (hm-ref heightmap (- x radius) (- y radius))
                                 (hm-ref heightmap (- x radius) (+ y radius))
                                 (hm-ref heightmap (+ x radius) (- y radius))
                                 (hm-ref heightmap (+ x radius) (+ y radius)))
                       spread)))

(defun ds-diamond (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average4 (hm-ref heightmap (- x radius) y)
                                 (hm-ref heightmap (+ x radius) y)
                                 (hm-ref heightmap x (- y radius))
                                 (hm-ref heightmap x (+ y radius)))
                       spread)))


(defun ds-squares (heightmap radius spread)
  (iterate
    (for x :from radius :below +world-size+ :by (* 2 radius))
    (iterate
      (for y :from radius :below +world-size+ :by (* 2 radius))
      (ds-square heightmap x y radius spread))))

(defun ds-diamonds (heightmap radius spread)
  (iterate
    (for i :from 0)
    (for y :from 0 :below +world-size+ :by radius)
    (iterate
      (with shift = (if (evenp i) radius 0))
      (for x :from shift :below +world-size+ :by (* 2 radius))
      (ds-diamond heightmap x y radius spread))))


(defun diamond-square (heightmap)
  (ds-init heightmap)
  (let ((spread 0.8)
        (spread-reduction 0.7))
    (recursively ((radius (floor +world-size+ 2))
                  (spread spread))
      (when (>= radius 1)
        (ds-squares heightmap radius spread)
        (ds-diamonds heightmap radius spread)
        (recur (/ radius 2)
               (* spread spread-reduction)))))
  (normalize-heightmap heightmap)
  heightmap)


;;;; Name Generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *name-syllables*
  (-> "syllables.txt"
    slurp
    read-from-string
    (coerce 'vector)))

(defun random-name ()
  (format nil "~:(~{~A~}~)"
          (iterate (repeat (random-range 1 5))
                   (collect (random-elt *name-syllables*)))))


;;;; Roll-Your-Own-ECS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entities are stored in an {id -> entity} hash table.
;;;
;;; Entities are also indexed by component in a nested hash table:
;;;
;;;     {component-symbol -> {id -> entity}}
;;;
;;; Entities are indexed by system too:
;;;
;;;     {system-symbol ->
;;;         ({id -> entity}   ; arg1
;;;          {id -> entity})  ; arg2
;;;     }
;;;
;;; Systems are stored as:
;;;
;;;     {system-symbol -> (cons system-function type-specifier-list)}
;;;
;;; TODO: Figure out the distinct problem.
;;; TODO: Unfuck redefining of systems.

(defvar *entity-id-counter* 0)
(defvar *entity-index* (make-hash-table))
(defvar *component-index* (make-hash-table))
(defvar *system-index* (make-hash-table))
(defvar *systems* (make-hash-table))


(defun get-entity (id)
  (gethash id *entity-index*))

(defun map-entities (function &optional (type 'entity))
  (->> *entity-index*
    hash-table-values
    (remove-if-not (lambda (entity) (typep entity type)))
    (mapcar function)))

(defun clear-entities ()
  (mapc #'destroy-entity (hash-table-values *entity-index*)))


(defun index-entity (entity)
  (setf (gethash (entity-id entity) *entity-index*) entity))

(defun satisfies-system-type-specifier-p (entity specifier)
  (every (lambda (component) (typep entity component))
         specifier))

(defun index-entity-systems (entity)
  (iterate
    (with id = (entity-id entity))
    (for (system (function . type-specifiers)) :in-hashtable *systems*)
    (iterate
      (for argument-index :in (gethash system *system-index*))
      (for specifier :in type-specifiers)
      (when (satisfies-system-type-specifier-p entity specifier)
        (setf (gethash id argument-index) entity)))))


(defclass entity ()
  ((id :reader entity-id :initform (incf *entity-id-counter*))))

(defmethod print-object ((e entity) stream)
  (print-unreadable-object (e stream :type t :identity nil)
    (format stream "~D" (entity-id e))))

(defmethod initialize-instance :after ((e entity) &key)
  (index-entity e)
  (index-entity-systems e))


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
      (remhash id index))
    (iterate
      (for (nil argument-indexes) :in-hashtable *system-index*)
      (iterate (for index :in argument-indexes)
               (remhash id index))))
  (entity-destroyed entity)
  nil)


(defmacro define-entity (name components &rest slots)
  `(defclass ,name (entity ,@components)
     (,@slots)))


(defun initialize-component-index (name)
  (gethash-or-init name *component-index* (make-hash-table)))

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
                      :initarg ,(intern (symbol-name field-name) "KEYWORD") ; *opens trenchcoat*
                      ,@field-options))))

      (defun ,(symbolize name '?) (object)
        (typep object ',name))

      (initialize-component-index ',name)

      (defmethod initialize-instance :after ((o ,name) &key)
        (setf (gethash (entity-id o)
                       (gethash ',name *component-index*))
              o))

      (find-class ',name))))


(defmacro define-system (name arglist &body body)
  `(progn
    (declaim (ftype (function
                      (,@(mapcar (lambda (arg)
                                   `(and entity ,@(cdr arg)))
                                 arglist))
                      (values null &optional))
                    ,name))
    (defun ,name (,@(mapcar #'car arglist))
      ,@body
      nil)
    (setf (gethash ',name *systems*)
          (cons #',name ',(mapcar #'cdr arglist))
          (gethash ',name *system-index*)
          (list ,@(iterate (repeat (length arglist))
                           (collect `(make-hash-table)))))
    ',name))

(defun run-system (system)
  (destructuring-bind (system-function . type-specifiers)
      (gethash system *systems*)
    (declare (ignore type-specifiers))
    (apply #'map-product system-function
           (mapcar #'hash-table-values (gethash system *system-index*)))
    (values)))


;;;; Components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates
(define-component coords
  (x :type world-coordinate)
  (y :type world-coordinate))


(deftype coordinate-array ()
  `(simple-array list (,+world-size+ ,+world-size+)))

(declaim (type coordinate-array *coords-contents*))
(defparameter *coords-contents*
  (make-array (list +world-size+ +world-size+) :initial-element nil))


(declaim (ftype (function (fixnum fixnum) list)
                coords-lookup)
         (ftype (function (coords) list)
                coords-insert-entity coords-remove-entity)
         (ftype (function (coords fixnum fixnum) list)
                coords-move-entity))

(defun coords-insert-entity (e)
  (push e (aref *coords-contents* (coords/x e) (coords/y e))))

(defun coords-remove-entity (e)
  (zap% (aref *coords-contents* (coords/x e) (coords/y e))
        #'delete e %))

(defun coords-move-entity (e new-x new-y)
  (coords-remove-entity e)
  (setf (coords/x e) (wrap new-x)
        (coords/y e) (wrap new-y))
  (coords-insert-entity e))

(defun coords-lookup (x y)
  (aref *coords-contents* (wrap x) (wrap y)))


(defmethod initialize-instance :after ((entity coords) &key)
  (zapf (coords/x entity) #'wrap
        (coords/y entity) #'wrap))

(defmethod entity-created :after ((entity coords))
  (coords-insert-entity entity))

(defmethod entity-destroyed :after ((entity coords))
  (coords-remove-entity entity))


;;; Flavor Text
(define-component flavor text)


;;; Inspection
(define-component inspectable slots)


;;; Visibility
(define-component visible glyph color)


;;; Food
(define-component edible
  energy)

(define-component decomposing
  rate
  (remaining :initform 1.0))

(define-component fruiting
  chance)


(define-system rot ((entity decomposing))
  (when (minusp (decf (decomposing/remaining entity)
                      (decomposing/rate entity)))
    (destroy-entity entity)))

(define-system rot-food ((entity decomposing edible))
  (mulf (edible/energy entity) 0.999))


(defun decomposing-description (entity)
  (let ((remaining (decomposing/remaining entity)))
    (cond
      ((< remaining 0.2) '("It is almost completely rotten."))
      ((< remaining 0.5) '("It is partially decomposed."))
      ((< remaining 0.8) '("It has begun to smell.")))))


;;;; Metabolism
(define-component metabolizing
  insulation
  energy)


(defgeneric starve (entity))

(defmethod starve ((entity entity))
  (destroy-entity entity))


(define-system consume-energy ((entity metabolizing))
  (when (minusp (decf (metabolizing/energy entity)))
    (starve entity)))


;;; Brains
(define-component sentient function)


(define-system sentient-act ((entity sentient))
  (funcall (sentient/function entity) entity))


;;; Age
(define-component aging
  (birthtick :initform *tick*)
  (age :initform 0))


(define-system age ((entity aging))
  (incf (aging/age entity)))


;;;; Entities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flora
(define-entity tree (coords visible fruiting flavor))
(define-entity fruit (coords visible edible flavor decomposing inspectable))
(define-entity algae (coords visible edible))


(defun make-tree (x y)
  (create-entity 'tree
                 :coords/x x
                 :coords/y y
                 :visible/glyph "T"
                 :visible/color +color-green+
                 :fruiting/chance 0.0005
                 :flavor/text '("A tree sways gently in the wind.")))

(defun make-fruit (x y)
  (create-entity 'fruit
                 :coords/x x
                 :coords/y y
                 :visible/glyph "ó"
                 :visible/color +color-pink+
                 :edible/energy (random-around 100 10)
                 :decomposing/rate 0.001
                 :inspectable/slots '(edible/energy)
                 :flavor/text '("A ripe piece of fruit has fallen to the ground.")))

(defun make-algae (x y)
  (create-entity 'algae
                 :coords/x x
                 :coords/y y
                 :edible/energy 10
                 :visible/glyph "`"
                 :visible/color +color-green+))


(define-system grow-fruit ((entity fruiting coords))
  (when (< (random 1.0) (fruiting/chance entity))
    (make-fruit (wrap (random-around (coords/x entity) 2))
                (wrap (random-around (coords/y entity) 2)))))


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


(defun generate-trees ()
  (iterate
    (for x :from 0 :below +world-size+)
    (iterate
      (for y :from 0 :below +world-size+)
      (when (< (random 1.0) (tree-probability x y))
        (make-tree x y)))))

(defun generate-algae ()
  (iterate
    (for x :from 0 :below +world-size+)
    (iterate
      (for y :from 0 :below +world-size+)
      (when (< (random 1.0) (algae-probability x y))
        (make-algae x y)))))


;;; Fauna
(define-entity creature
    (coords visible sentient flavor metabolizing aging inspectable)
  (name :accessor creature-name :initarg :name))

(define-entity corpse
    (coords visible flavor decomposing))


(defparameter *directions*
  (iterate dirs (for dx :from -1 :to 1)
           (iterate (for dy :from -1 :to 1)
                    (in dirs (collect (cons dx dy) :result-type 'vector)))))

(defun nearby (entity)
  (remove entity
          (iterate
            outer
            (with r = 1)
            (with x = (coords/x entity))
            (with y = (coords/y entity))
            (for dx :from (- r) :to r)
            (iterate
              (for dy :from (- r) :to r)
              (in outer
                  (appending (coords-lookup (+ x dx)
                                            (+ y dy))))))))


(defun creature-move (c)
  (let ((x (coords/x c))
        (y (coords/y c)))
    (destructuring-bind (dx . dy)
        (random-elt *directions*)
      (coords-move-entity c (+ x dx) (+ y dy)))))

(defun creature-eat (c food)
  (incf (metabolizing/energy c) (edible/energy food))
  (destroy-entity food))

(defun creature-act (c)
  (let* ((near (nearby c))
         (food (find-if #'edible? near)))
    (if food
      (creature-eat c food)
      (creature-move c))))


(defun make-creature (x y)
  (let ((name (random-name)))
    (create-entity
      'creature
      :name name
      :coords/x x
      :coords/y y
      :visible/color +color-white+
      :visible/glyph "@"
      :metabolizing/energy 1000
      :metabolizing/insulation 1
      :sentient/function 'creature-act
      :inspectable/slots '(metabolizing/energy aging/birthtick aging/age)
      :flavor/text (list (format nil "A creature named ~:(~A~) is here." name)
                         "It likes food."))))

(defun make-corpse (x y name)
  (create-entity
    'corpse
    :coords/x x
    :coords/y y
    :visible/color +color-white+
    :visible/glyph "%"
    :decomposing/rate 0.001
    :flavor/text (list (format nil "The corpse of ~:(~A~) lies here." name))))


(defmethod starve :after ((c creature))
  (log-message "~A has starved after living for ~D tick~:P."
               (creature-name c)
               (aging/age c))
  (make-corpse (coords/x c)
               (coords/y c)
               (creature-name c)))


(defmethod entity-created :after ((e creature))
  (declare (ignore e))
  (incf *population*))

(defmethod entity-destroyed :after ((e creature))
  (declare (ignore e))
  (decf *population*))


;;; Mysteries
(define-entity monolith (coords visible sentient flavor)
  (countdown :initarg :countdown :accessor monolith-countdown))


(defun monolith-act (m)
  (when (zerop *population*)
    (case (decf (monolith-countdown m))
      (40 (log-message "The monolith begins to glow."))
      (0 (progn
           (setf (monolith-countdown m) 100)
           (-<> (make-creature (coords/x m) (1+ (coords/y m)))
             creature-name
             (log-message
               "The monolith flashes brightly and ~A appears in front of it!"
               <>)))))))

(defun make-monolith ()
  (create-entity 'monolith
                 :countdown 50
                 :coords/x 0
                 :coords/y 0
                 :visible/glyph " "
                 :visible/color +color-orange+
                 :sentient/function 'monolith-act
                 :flavor/text
                 '("A sleek, rectangular, octarine monolith stands here."
                   "Who placed it?")))


(defun generate-mysteries ()
  (make-monolith))


;;;; Profiling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dump-profile ()
  (with-open-file (*standard-output* "silt.prof"
                                     :direction :output
                                     :if-exists :supersede)
    (sb-sprof:report :type :graph
                     :sort-by :cumulative-samples
                     :sort-order :ascending)
    (sb-sprof:report :type :flat
                     :min-percent 0.5)))

(defun start-profiling ()
  (sb-sprof::reset)
  (sb-sprof::profile-call-counts "SILT")
  (sb-sprof::start-profiling :max-samples 50000
                             :mode :cpu
                             :sample-interval 0.005
                             :threads :all))

(defun stop-profiling ()
  (sb-sprof::stop-profiling)
  (dump-profile))


;;;; Game State Machine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *controls-text*
  '("CONTROLS"
    "  hjklyubn - move your view"
    "  HJKLYUBN - move your view faster"
    ""
    "  wasd - move your cursor"
    "  WASD - move your cursor faster"
    ""
    "  space - pause time"
    "  ` - tick time once while paused"
    ""
    "  Q - quit"
    "  R - regenerate the world"
    ""
    "  ? - help"))

(defun render-title ()
  (render
    (write-centered '("S I L T"
                      ""
                      "Press any key to start...")
                    *screen-center-x*
                    (1- *screen-center-y*))))

(defun render-intro ()
  (render
    (write-left (append '("Welcome to Silt."
                          ""
                          "You are the god of a toroidal world."
                          ""
                          "Move your cursor over things to observe them."
                          "")
                        *controls-text*
                        '(""
                          "Press any key to begin."))
                1 1)))

(defun render-help ()
  (render
    (write-left (append *controls-text*
                        '(""
                          "Press any key to continue."))
                1 1)))

(defun render-generate ()
  (render
    (write-centered "Generating world, please wait..."
                    *screen-center-x* *screen-center-y*)))


(defun draw-map ()
  (iterate
    (repeat *screen-width*)
    (for sx :from 0)
    (for wx :from *view-x*)
    (iterate
      (repeat *screen-height*)
      (for sy :from 0)
      (for wy :from *view-y*)
      (for entity = (find-if #'visible? (coords-lookup wx wy)))
      (if entity
        (with-color (visible/color entity)
          (write-string-at (visible/glyph entity) sx sy))
        (multiple-value-bind (glyph color) (terrain-char wx wy)
          (with-color color
            (write-char-at glyph sx sy)))))))

(defun draw-hud ()
  (write-right
    (list
      (format nil "[~D, ~D]" *view-x* *view-y*)
      (format nil "[~D, ~D]" *cursor-x* *cursor-y*)
      (format nil "~D creature~:P" *population*)
      (format nil "~D entit~:@P" (hash-table-count *entity-index*))
      (format nil "tick ~D" *tick*)
      (if (equal *timing* (cons 0 0))
        ""
        (format nil "~,5Fms per run over ~D runs"
                (/ (cdr *timing*) internal-time-units-per-second 1/1000)
                (car *timing*))))
    (1- *screen-width*)
    1))

(defun draw-paused ()
  (when *paused*
    (write-right '("            "
                   "   PAUSED   "
                   "            ")
                 *screen-width*
                 (- *screen-height* 3))))

(defun draw-log ()
  (let ((messages *game-log*))
    (write-left (nreverse (mapcar #'cdr messages))
                0
                (- *screen-height* (length messages)))))


(defun indent (lines)
  (mapcar (lambda (line) (concatenate 'string "    " line)) lines))

(defun draw-selected ()
  (write-left
    (iterate
      (for entity :in (multiple-value-call #'coords-lookup
                        (screen-to-world *cursor-x* *cursor-y*)))
      (when (typep entity 'flavor)
        (appending (flavor/text entity) :into text)

        (when (typep entity 'decomposing)
          (appending (indent (decomposing-description entity)) :into text))

        (when (typep entity 'inspectable)
          (appending
            (indent (iterate
                      (for slot :in (inspectable/slots entity))
                      (collect (format nil "~A ~A" slot (funcall slot entity)))))
            :into text))

        (collecting "" :into text))
      (finally (return text)))
    1 1))


(defun draw-ui ()
  (draw-hud)
  (draw-selected)
  (draw-paused)
  (draw-log))


(defun render-map ()
  (manage-screen)
  (draw-map)
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

      ((#\Space) (zapf *paused* #'not))
      ((#\`) (when *paused* (tick-world)))

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

      ((#\w) (move-cursor   0  -1))
      ((#\a) (move-cursor  -1   0))
      ((#\s) (move-cursor   0   1))
      ((#\d) (move-cursor   1   0))
      ((#\W) (move-cursor   0 -10))
      ((#\A) (move-cursor -10   0))
      ((#\S) (move-cursor   0  10))
      ((#\D) (move-cursor  10   0)))))


(defun tick-world ()
  (incf *tick*)
  (run-system 'age)
  (run-system 'consume-energy)
  (run-system 'grow-fruit)
  (run-system 'rot)
  (run-system 'rot-food)
  (run-system 'sentient-act))

(defun tick-log ()
  (flet ((decrement (message)
           (decf (car message)))
         (dead (message)
           (zerop (car message))))
    (setf *game-log*
          (->> *game-log*
            (mapc #'decrement)
            (remove-if #'dead)))))


(defun state-title ()
  (render-title)
  (press-any-key)
  (state-intro))

(defun state-intro ()
  (render-intro)
  (press-any-key)
  (state-generate))


(defun reset-world ()
  (setf *random-state* (make-random-state t))
  (clear-entities)
  (setf *view-x* (wrap (- *screen-center-x*))
        *view-y* (wrap (- *screen-center-y*))
        *cursor-x* 0
        *cursor-y* 0
        *population* 0
        *tick* 0
        *paused* nil))

(defun generate-world ()
  (setf *heightmap* (diamond-square (allocate-heightmap)))
  (generate-trees)
  (generate-algae)
  (generate-mysteries))

(defun state-generate ()
  (manage-screen)
  (render-generate)
  (reset-world)
  (generate-world)
  (state-map))


(defun state-map ()
  (charms:enable-non-blocking-mode charms:*standard-window*)
  (state-map-loop))

(defun state-map-loop ()
  (case (handle-input-map)
    ((:quit) (state-quit))
    ((:regen) (state-generate))
    ((:help) (state-help))
    (t (progn
         (unless *paused*
           (tick-world)
           (tick-log))
         (render-map)
         (sleep 0.02)
         (state-map-loop)))))


(defun state-help ()
  (render-help)
  (press-any-key)
  (state-map))

(defun state-quit ()
  'goodbye)


;;;; Run ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run ()
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


;;;; Scratch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (run)
; (start-profiling)
; (stop-profiling)

