(in-package #:silt)
#+sbcl (require :sb-sprof)

; (declaim (optimize (speed 3) (debug 0) (safety 0)))
; (declaim (optimize (speed 3) (debug 0) (safety 1)))
; (declaim (optimize (speed 1) (debug 1) (safety 1)))


;;;; Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-constant +world-exponent+ 10)
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
(defparameter *temperature* 0)
(defparameter *frame-skip* 1)
(defparameter *sleep* t)


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
(defmacro defcolors (&rest colors)
  `(progn
    ,@(iterate (for n :from 0)
               (for (constant nil nil) :in colors)
               (collect `(define-constant ,constant ,n)))
    (defun init-colors ()
      ,@(iterate
          (for (constant fg bg) :in colors)
          (collect `(charms/ll:init-pair ,constant ,fg ,bg))))))

(defcolors
  (+color-white-black+  charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
  (+color-blue-black+   charms/ll:COLOR_BLUE    charms/ll:COLOR_BLACK)
  (+color-cyan-black+   charms/ll:COLOR_CYAN    charms/ll:COLOR_BLACK)
  (+color-yellow-black+ charms/ll:COLOR_YELLOW  charms/ll:COLOR_BLACK)
  (+color-green-black+  charms/ll:COLOR_GREEN   charms/ll:COLOR_BLACK)
  (+color-pink-black+   charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK)

  (+color-black-white+  charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)
  (+color-black-yellow+ charms/ll:COLOR_BLACK charms/ll:COLOR_YELLOW)

  (+color-white-blue+   charms/ll:COLOR_WHITE charms/ll:COLOR_BLUE)

  (+color-white-red+    charms/ll:COLOR_WHITE charms/ll:COLOR_RED)

  (+color-white-green+  charms/ll:COLOR_WHITE charms/ll:COLOR_GREEN))

(defmacro with-color (color &body body)
  (once-only (color)
    `(unwind-protect
      (progn
        (charms/ll:attron (charms/ll:color-pair ,color))
        ,@body)
      (charms/ll:attroff (charms/ll:color-pair ,color)))))


;;;; Ticklists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-ticklist ()
  nil)

(defmacro ticklist-push (ticklist value lifespan)
  `(push (cons ,lifespan ,value) ,ticklist))

(defun ticklist-tick (ticklist)
  (flet ((decrement (entry)
           (decf (car entry)))
         (dead (entry)
           (minusp (car entry))))
    (-<> ticklist
      (mapc #'decrement <>)
      (remove-if #'dead <>))))

(defun ticklist-contents (ticklist)
  (mapcar #'cdr ticklist))


;;;; Weightlists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (weightlist (:constructor %make-weightlist))
  weights sums items total)

(defun make-weightlist (items weights)
  "Make a weightlist of the given items and weights."
  (%make-weightlist
    :items items
    :weights weights
    :sums (prefix-sums weights)
    :total (apply #'+ weights)))

(defun weightlist-random (weightlist)
  "Return a random item from the weightlist, taking the weights into account."
  (iterate
    (with n = (random (weightlist-total weightlist)))
    (for item :in (weightlist-items weightlist))
    (for weight :in (weightlist-sums weightlist))
    (finding item :such-that (< n weight))))


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


(defun left-pad (s n)
  (format nil "~v@A" n s))

(defun right-pad (s n)
  (format nil "~vA" n s))


(defun write-centered (text x y)
  (etypecase text
    (string (write-centered (list text) x y))
    (list (iterate
            (for string :in text)
            (for tx = (- x (floor (length string) 2)))
            (for ty :from y)
            (write-string-at string tx ty)))))

(defun write-left (text x y &key pad)
  (etypecase text
    (string (write-left (list text) x y))
    (list (iterate
            (with padding = (if (and pad text)
                              (apply #'max (mapcar #'length text))
                              0))
            (for string :in text)
            (for tx = x)
            (for ty :from y)
            (unless (string= "" string)
              (write-string-at (right-pad string padding) tx ty))))))

(defun write-right (text x y &key pad)
  (etypecase text
    (string (write-right (list text) x y))
    (list (iterate
            (with padding = (if (and pad text)
                              (apply #'max (mapcar #'length text))
                              0))
            (for string :in text)
            (for tx = (- x (max padding (length string))))
            (for ty :from y)
            (unless (string= "" string)
              (write-string-at (left-pad string padding) tx ty))))))


(defun log-height ()
  (max 5 (floor *screen-height* 4)))


(defun log-message (s &rest args)
  (ticklist-push *game-log* (apply #'format nil s args) 200)
  (setf *game-log* (take (log-height) *game-log*)))


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
    (:deep-water    (values #\~ +color-blue-black+))
    (:shallow-water (values #\~ +color-cyan-black+))
    (:sand          (values #\: +color-yellow-black+))
    (:grass         (values #\. +color-green-black+))
    (:dirt          (values #\. +color-white-black+))
    (:hills         (values #\^ +color-white-black+))
    (:mountain      (values #\# +color-white-black+))
    (:snow          (values #\* +color-black-white+))))


(defun random-coordinate (&optional terrain-type)
  (iterate
    (repeat 10000)
    (for x = (random +world-size+))
    (for y = (random +world-size+))
    (finding (cons x y) :such-that (or (null terrain-type)
                                       (eql terrain-type (terrain-type x y))))))


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


(defmacro modf (place n)
  `(zapf ,place (mod % ,n)))


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
  (-<> "syllables.txt"
    alexandria:read-file-into-string
    read-from-string
    (coerce <> 'vector)))

(defun random-name ()
  (format nil "~:(~{~A~}~)"
          (iterate (repeat (random-range 1 5))
                   (collect (random-elt *name-syllables*)))))


;;;; Aspects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates
(define-aspect coords
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
  (zapf (aref *coords-contents* (coords/x e) (coords/y e))
        (delete e %)))

(defun coords-move-entity (e new-x new-y)
  (coords-remove-entity e)
  (setf (coords/x e) (wrap new-x)
        (coords/y e) (wrap new-y))
  (coords-insert-entity e))

(defun coords-lookup (x y)
  (aref *coords-contents* (wrap x) (wrap y)))


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


(defmethod initialize-instance :after ((entity coords) &key)
  (callf (coords/x entity) #'wrap
        (coords/y entity) #'wrap))

(defmethod entity-created :after ((entity coords))
  (coords-insert-entity entity))

(defmethod entity-destroyed :after ((entity coords))
  (coords-remove-entity entity))


;;; Flavor Text
(define-aspect flavor text)


;;; Inspection
(define-aspect inspectable
  (slots :initform nil))

(defun inspectable-get (entity slot)
  (etypecase slot
    (symbol (cons slot (slot-value entity slot)))
    (function (funcall slot entity))))


;;; Visibility
(define-aspect visible glyph color)


;;; Food
(define-aspect edible
  energy
  original-energy)

(define-aspect decomposing
  rate
  (remaining :initform 1.0))

(define-aspect fruiting
  chance)


(defmethod initialize-instance :after ((e edible) &key)
  (setf (edible/original-energy e)
        (edible/energy e)))


(define-system (rot :inline t) ((entity decomposing))
  (when (minusp (decf (decomposing/remaining entity)
                      (decomposing/rate entity)))
    (destroy-entity entity)))

(define-system (rot-food :inline t) ((entity decomposing edible))
  (setf (edible/energy entity)
        (lerp 0.0 (edible/original-energy entity)
              (decomposing/remaining entity))))


(defun decomposing-description (entity)
  (let ((remaining (decomposing/remaining entity)))
    (cond
      ((< remaining 0.2) '("It is almost completely rotten."))
      ((< remaining 0.5) '("It is partially decomposed."))
      ((< remaining 0.8) '("It has begun to smell.")))))


;;;; Metabolism
(define-aspect metabolizing
  insulation
  energy)


(defgeneric starve (entity))

(defmethod starve ((entity entity))
  (destroy-entity entity))


(defgeneric calculate-energy-cost (entity))

(defmethod calculate-energy-cost ((entity metabolizing))
  (let* ((insulation (metabolizing/insulation entity))
         (base-cost 1.0)
         (temperature-cost (max 0 (* 0.2 (- (abs *temperature*) insulation))))
         (insulation-cost (* 0.1 insulation)))
    (+ base-cost temperature-cost insulation-cost)))


(define-system consume-energy ((entity metabolizing))
  (when (minusp (decf (metabolizing/energy entity)
                      (calculate-energy-cost entity)))
    (starve entity)))


;;; Brains
(define-aspect sentient function)
(define-aspect periodic
  function
  (counter :initform 1)
  next
  min
  max)


(define-system sentient-act ((entity sentient))
  (funcall (sentient/function entity) entity))

(define-system periodic-tick ((entity periodic))
  (when (zerop (setf (periodic/counter entity)
                     (mod (1+ (periodic/counter entity))
                          (periodic/next entity))))
    (setf (periodic/next entity)
          (random-range (periodic/min entity)
                        (periodic/max entity)))
    (funcall (periodic/function entity) entity)))


;;; Age
(define-aspect aging
  (birthtick :initform *tick*)
  (age :initform 0))


(define-system age ((entity aging))
  (incf (aging/age entity)))


;;;; Entities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flora
(define-entity tree (coords visible fruiting flavor))
(define-entity fruit (coords visible edible flavor decomposing inspectable))
(define-entity algae (coords visible edible decomposing))
(define-entity grass (coords visible edible decomposing))


(defun make-tree (x y)
  (create-entity 'tree
                 :coords/x x
                 :coords/y y
                 :visible/glyph "T"
                 :visible/color +color-green-black+
                 :fruiting/chance 0.0005
                 :flavor/text '("A tree sways gently in the wind.")))

(defun make-fruit (x y)
  (create-entity 'fruit
                 :coords/x x
                 :coords/y y
                 :visible/glyph "ó"
                 :visible/color +color-pink-black+
                 :edible/energy (random-around 500 10)
                 :decomposing/rate 0.0005
                 :inspectable/slots '(edible/energy)
                 :flavor/text '("A ripe piece of fruit has fallen to the ground.")))

(defun make-algae (x y)
  (create-entity 'algae
                 :coords/x x
                 :coords/y y
                 :edible/energy 50
                 :decomposing/rate 0.003
                 :visible/glyph "`"
                 :visible/color +color-green-black+))

(defun make-grass (x y)
  (create-entity 'grass
                 :coords/x x
                 :coords/y y
                 :edible/energy 20
                 :decomposing/rate 0.001
                 :visible/glyph "\""
                 :visible/color +color-green-black+))


(define-system grow-fruit ((entity fruiting coords))
  (when (randomp (fruiting/chance entity))
    (make-fruit (wrap (random-around (coords/x entity) 2))
                (wrap (random-around (coords/y entity) 2)))))


(defun tree-probability (x y)
  (case (terrain-type x y)
    (:grass 0.01)
    (:dirt 0.001)
    (t 0)))


(defun generate-trees ()
  (iterate
    (for x :from 0 :below +world-size+)
    (iterate
      (for y :from 0 :below +world-size+)
      (when (randomp (tree-probability x y))
        (make-tree x y)))))


(defun grow-algae ()
  (iterate
    (repeat (floor (* +world-size+ +world-size+) 100000))
    (let ((target (random-coordinate :shallow-water)))
      (when target (make-algae (car target) (cdr target))))))

(defun grow-grass ()
  (iterate
    (repeat (floor (* +world-size+ +world-size+) 100000))
    (let ((target (random-coordinate :grass)))
      (when target (make-grass (car target) (cdr target))))))


;;; Fauna
(define-entity creature
    (coords visible sentient flavor metabolizing aging inspectable)
  (name :accessor creature-name :initarg :name)
  (directions :accessor creature-directions :initarg :directions))

(define-entity corpse
  (coords visible flavor decomposing))


(defparameter *directions*
  (iterate dirs (for dx :from -1 :to 1)
           (iterate (for dy :from -1 :to 1)
                    (in dirs (collect (cons dx dy))))))

(defparameter *default-creature-directions*
  (make-weightlist *directions*
                   (iterate (repeat (length *directions*))
                            (collect 10))))

(defparameter *creature-colors*
  (vector +color-white-black+
          +color-blue-black+
          +color-cyan-black+
          +color-yellow-black+
          +color-green-black+
          +color-pink-black+))

(defparameter *creature-glyphs*
  (vector "@" "$" "?" "!" "&" "+" "☃" "$" "&" "¥"
          "£" "¤" "€" "‡" "¶" "µ" "¢" "¬" "¿" "§"))


(defun creature-mutate-glyph (c)
  (setf (visible/glyph c) (random-elt *creature-glyphs*)))

(defun creature-mutate-color (c)
  (setf (visible/color c) (random-elt *creature-colors*)))

(defun creature-mutate-directions (c)
  (let ((old (creature-directions c)))
    (setf (creature-directions c)
          (make-weightlist (weightlist-items old)
                           (mapcar (lambda (w)
                                     (max 1 (random-around w 1)))
                                   (weightlist-weights old))))))

(defun creature-mutate-appearance (c)
  (if (randomp)
    (creature-mutate-color c)
    (creature-mutate-glyph c)))

(defun creature-mutate-insulation (c)
  (setf (metabolizing/insulation c)
        (max 0 (random-around (metabolizing/insulation c) 1))))

(defun creature-mutate (c)
  (let ((v (random 1.0)))
    (cond
      ((< v 0.40) (creature-mutate-directions c))
      ((< v 0.90) (creature-mutate-insulation c))
      ((< v 0.99) (creature-mutate-color c))
      ((< v 1.00) (creature-mutate-glyph c)))))


(defun creature-should-reproduce-p (c)
  (and (> (metabolizing/energy c) 1000)
       (randomp 0.01)))

(defun creature-should-mutate-p ()
  (randomp 0.6))

(defun creature-reproduce (parent)
  (let* ((energy (floor (metabolizing/energy parent) 2))
         (child (make-creature (coords/x parent) (coords/y parent)
                               :color (visible/color parent)
                               :glyph (visible/glyph parent)
                               :energy energy
                               :insulation (metabolizing/insulation parent)
                               :directions (creature-directions parent))))
    (setf (metabolizing/energy parent) energy)
    (when (creature-should-mutate-p)
      (creature-mutate child))
    (log-message "~A begets ~A." (creature-name parent) (creature-name child))))


(defun creature-move (c)
  (let ((x (coords/x c))
        (y (coords/y c)))
    (destructuring-bind (dx . dy)
        (weightlist-random (creature-directions c))
      (coords-move-entity c (+ x dx) (+ y dy)))))

(defun creature-eat (c food)
  (incf (metabolizing/energy c) (edible/energy food))
  (destroy-entity food))

(defun creature-act (c)
  (let* ((near (nearby c))
         (food (find-if #'edible? near)))
    (cond
      (food (creature-eat c food))
      ((creature-should-reproduce-p c) (creature-reproduce c))
      (t (creature-move c)))))


(defun make-creature (x y &key
                      (directions *default-creature-directions*)
                      (color +color-white-black+)
                      (glyph "@")
                      (energy 3000)
                      (insulation 0))
  (let ((name (random-name)))
    (create-entity
      'creature
      :name name
      :directions directions
      :coords/x x
      :coords/y y
      :visible/color color
      :visible/glyph glyph
      :metabolizing/energy energy
      :metabolizing/insulation insulation
      :sentient/function 'creature-act
      :inspectable/slots
      (list 'name
            (lambda (c) (cons 'directions
                              (weightlist-weights (creature-directions c))))
            'metabolizing/energy 'metabolizing/insulation
            'aging/birthtick 'aging/age)
      :flavor/text
      (list (format nil "A creature named ~:(~A~) is here." name)
            "It likes food."))))

(defun make-corpse (x y color name)
  (create-entity
    'corpse
    :coords/x x
    :coords/y y
    :visible/color color
    :visible/glyph "%"
    :decomposing/rate 0.001
    :flavor/text (list (format nil "The corpse of ~:(~A~) lies here." name))))


(defmethod starve :after ((c creature))
  (log-message "~A has starved after living for ~D tick~:P."
               (creature-name c)
               (aging/age c))
  (make-corpse (coords/x c)
               (coords/y c)
               (visible/color c)
               (creature-name c)))


(defmethod entity-created :after ((e creature))
  (declare (ignore e))
  (incf *population*))

(defmethod entity-destroyed :after ((e creature))
  (declare (ignore e))
  (decf *population*))


;;; Mysteries
(define-entity monolith (coords visible sentient flavor)
  (countdown :initarg :countdown))

(define-entity fountain (coords visible sentient flavor)
  (recent :initform (make-ticklist)))

(define-entity colossus (coords visible periodic flavor))
(define-entity yggdrasil (coords visible periodic flavor))
(define-entity yggdrasil-sapling (coords visible flavor))


(defun monolith-act (m)
  (when (zerop *population*)
    (with-slots (countdown) m
        (case (decf countdown)
          (40 (log-message "The monolith begins to glow."))
          (0 (progn
               (setf countdown 100)
               (-<> (make-creature (coords/x m) (1+ (coords/y m)))
                 creature-name
                 (log-message
                   "The monolith flashes brightly and ~A appears in front of it!"
                   <>))))))))

(defun fountain-act (f)
  (with-slots (recent) f
    (callf recent #'ticklist-tick)
    (iterate
      (for creature :in (remove-if-not #'creature? (nearby f)))
      (unless (member creature (ticklist-contents recent))
        (creature-mutate-appearance creature)
        (ticklist-push recent creature 1000)
        (log-message "~A drinks from the fountain and... changes."
                     (creature-name creature))))))

(defun colossus-act (c)
  (coords-move-entity c (1+ (coords/x c)) (coords/y c))
  (log-message "The colossus takes a step."))

(defun yggdrasil-act (ygg)
  (let* ((x (coords/x ygg))
         (y (coords/y ygg))
         (nx (random-gaussian-integer x 10))
         (ny (random-gaussian-integer y 10)))
    (unless (or (and (= x nx) (= y ny))
                (find-if #'yggdrasil-sapling? (coords-lookup nx ny)))
      (log-message "A leaf falls from the massive ash tree and flutters to the ground.")
      (log-message "A new tree springs up where it fell!")
      (make-yggdrasil-sapling nx ny))))


(defun make-monolith ()
  (create-entity 'monolith
                 :countdown 50
                 :coords/x 0
                 :coords/y 0
                 :visible/glyph " "
                 :visible/color +color-black-yellow+
                 :sentient/function 'monolith-act
                 :flavor/text
                 '("A sleek, rectangular, octarine monolith stands here."
                   "Who placed it?")))

(defun make-fountain ()
  (let ((loc (random-coordinate :grass)))
    (when loc
      (create-entity 'fountain
                 :coords/x (car loc)
                 :coords/y (cdr loc)
                 :visible/glyph "ƒ"
                 :visible/color +color-white-blue+
                 :sentient/function 'fountain-act
                 :flavor/text
                 '("A marble fountain burbles peacefully.")))))

(defun make-colossus ()
  (let ((loc (random-coordinate :snow)))
    (when loc
      (create-entity 'colossus
                     :coords/x (car loc)
                     :coords/y (cdr loc)
                     :visible/glyph "@"
                     :visible/color +color-white-red+
                     :periodic/function 'colossus-act
                     :periodic/next 1000
                     :periodic/min 2000
                     :periodic/max 10000
                     :flavor/text
                     '("A massive granite statue of an alien being.")))))

(defun make-yggdrasil ()
  (let ((loc (random-coordinate :grass)))
    (when loc
      (create-entity 'yggdrasil
                     :coords/x (car loc)
                     :coords/y (cdr loc)
                     :visible/glyph "Y"
                     :visible/color +color-white-green+
                     :periodic/function 'yggdrasil-act
                     :periodic/next 600
                     :periodic/min 1000
                     :periodic/max 2000
                     :flavor/text
                     '("An immense ash tree."
                       "Its branches touch the stars.")))))

(defun make-yggdrasil-sapling (x y)
  (create-entity 'yggdrasil-sapling
                 :coords/x x
                 :coords/y y
                 :visible/glyph "y"
                 :visible/color +color-green-black+
                 :flavor/text '("An small ash tree bends toward the sun.")))


(defun generate-mysteries ()
  (make-colossus)
  (make-fountain)
  (make-yggdrasil)
  (make-monolith))


;;;; Profiling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+sbcl
(defun dump-profile ()
  (with-open-file (*standard-output* "silt.prof"
                                     :direction :output
                                     :if-exists :supersede)
    (sb-sprof:report :type :graph
                     :sort-by :cumulative-samples
                     :sort-order :ascending)
    (sb-sprof:report :type :flat
                     :min-percent 0.5)))

#+sbcl
(defun start-profiling ()
  (sb-sprof::reset)
  (sb-sprof::profile-call-counts "SILT")
  (sb-sprof::start-profiling :max-samples 50000
                             ; :mode :cpu
                             :mode :time
                             :sample-interval 0.01
                             :threads :all))

#+sbcl
(defun stop-profiling ()
  (sb-sprof::stop-profiling)
  (dump-profile))


;;;; Game State Machine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *controls-text*
  '("CONTROLS"
    "  [hjklyubn] move your view"
    "  [HJKLYUBN] move your view faster"
    ""
    "      [wasd] move your cursor"
    "      [WASD] move your cursor faster"
    ""
    "         [+] increase the temperature of the world"
    "         [-] decrease the temperature of the world"
    ""
    "     [space] pause time"
    "         [`] tick time once while paused"
    ""
    "         [Q] quit"
    "         [R] regenerate the world"
    ""
    "         [?]  help"))

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
      (format nil "~D entit~:@P" (hash-table-count beast::*entity-index*))
      (format nil "~D°" *temperature*)
      (format nil "tick ~D" *tick*)
      (if (equal *timing* (cons 0 0))
        ""
        (format nil "~,5Fms per run over ~D runs"
                (/ (cdr *timing*) internal-time-units-per-second 1/1000)
                (car *timing*)))
      (if (= *frame-skip* 1)
        ""
        (format nil "frameskip: ~D" *frame-skip*)))
    (1- *screen-width*)
    1
    :pad t))

(defun draw-paused ()
  (when *paused*
    (write-right '("            "
                   "   PAUSED   "
                   "            ")
                 *screen-width*
                 (- *screen-height* 3))))

(defun draw-log ()
  (let ((messages (nreverse (ticklist-contents *game-log*))))
    (write-left messages 0 (- *screen-height* (length messages)))))


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
            (indent
              (iterate
                (with slots = (mapcar (curry #'inspectable-get entity)
                                      (inspectable/slots entity)))
                (with width = (apply #'max
                                     (mapcar (compose #'length #'symbol-name #'car)
                                             slots)))
                (for (label . contents) :in slots)
                (collect
                  (let ((*print-pretty* nil))
                    (format nil "~vA ~A" width label contents)))))
            :into text))

        (collecting "" :into text))
      (finally (return text)))
    1 1 :pad t))


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

      ((#\Space) (callf *paused* #'not))
      ((#\`) (when *paused* (tick-world)))

      ((#\+) (incf *temperature*))
      ((#\-) (decf *temperature*))

      ((#\]) (incf *frame-skip*))
      ((#\[) (setf *frame-skip* (clamp 1 100 (1- *frame-skip*))))
      ((#\!) (callf *sleep* #'not))

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


(defun tick-flora ()
  (run-grow-fruit)
  (grow-algae)
  (grow-grass)
  (run-rot)
  (run-rot-food))

(defun tick-world ()
  (incf *tick*)
  (run-age)
  (run-consume-energy)
  (tick-flora)
  (run-sentient-act)
  (run-periodic-tick))

(defun tick-log ()
  (setf *game-log* (ticklist-tick *game-log*)))


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
        *game-log* (make-ticklist)
        *cursor-x* 0
        *cursor-y* 0
        *population* 0
        *tick* 0
        *temperature* 0
        *frame-skip* 1
        *paused* nil))

(defun generate-world ()
  (setf *heightmap* (diamond-square (allocate-heightmap)))
  (generate-trees)
  (generate-mysteries)
  (iterate (repeat 1000)
           (tick-flora)))

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
           (iterate (repeat *frame-skip*)
                    (tick-world)
                    (tick-log)))
         (render-map)
         (when *sleep*
           (sleep 0.05))
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

