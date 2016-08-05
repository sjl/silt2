(in-package #:silt.utils)

;;;; Miscellaneous
(defmacro zap% (place function &rest arguments &environment env)
  "Update `place` by applying `function` to its current value and `arguments`.

  `arguments` should contain the symbol `%`, which is treated as a placeholder
  where the current value of the place will be substituted into the function
  call.

  For example:

  (zap% foo #'- % 10) => (setf foo (- foo 10)
  (zap% foo #'- 10 %) => (setf foo (- 10 foo)

  "
  ;; original idea/name from http://malisper.me/2015/09/29/zap/
  (assert (find '% arguments) ()
    "Placeholder % not included in zap macro form.")
  (multiple-value-bind (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
             (funcall ,function
                      ,@(substitute access-expr '% arguments))))
      ,store-expr)))

(defmacro recursively (bindings &body body)
  "Execute body recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) default values.

  In `body`, `recur` will be bound to the function for recurring.

  Example:

      (defun length (some-list)
        (recursively ((list some-list) (n 0))
          (if (null list)
            n
            (recur (cdr list) (1+ n)))))

  "
  (flet ((extract-var (binding)
           (if (atom binding) binding (first binding)))
         (extract-val (binding)
           (if (atom binding) nil (second binding))))
    `(labels ((recur ,(mapcar #'extract-var bindings)
                ,@body))
      (recur ,@(mapcar #'extract-val bindings)))))

(defmacro dis (arglist &body body)
  "Disassemble the code generated for a `lambda*` with `arglist` and `body`.

  It will also spew compiler notes so you can see why the garbage box isn't
  doing what you think it should be doing.

  "
  `(->> '(lambda ,arglist
          (declare (optimize speed))
          ,@body)
    (compile nil)
    #+sbcl sb-disassem:disassemble-code-component
    #-sbcl disassemble))

(defmacro spit (filename &body body)
  `(with-open-file (*standard-output* ,filename
                                      :direction :output
                                      :if-exists :supersede)
     ,@body))

(defun symbolize (&rest args)
  (intern (format nil "~{~A~}" args)))


;;;; Maths
(defun d (n sides &optional (plus 0))
  "Roll some dice.

  (d 1 4)     -> roll 1d4
  (d 2 8)     -> roll 2d8
  (d 1 10 -1) -> roll 1d10-1

  "
  (+ (iterate (repeat n)
              (sum (1+ (random sides))))
     plus))

(defun clamp (from to n)
  (let ((max (max from to))
        (min (min from to)))
    (cond
      ((> n max) max)
      ((< n min) min)
      (t n))))


;;;; dlambda
(defmacro dlambda (&rest clauses)
  (with-gensyms (message arguments)
    (flet ((parse-clause (clause)
             (destructuring-bind (key arglist &rest body)
                 clause
               `(,key (apply (lambda ,arglist ,@body) ,arguments)))))
      `(lambda (,message &rest ,arguments)
        (ecase ,message
          ,@(mapcar #'parse-clause clauses))))))


;;;; Sets
;;; Janky implementation of basic sets.
(defclass hash-set ()
  ((data :initarg :data)))


(defun make-set (&key (test #'eql) (initial-data nil))
  (let ((set (make-instance 'hash-set
                            :data (make-hash-table :test test))))
    (mapcar (curry #'set-add set) initial-data)
    set))


(defun set-contains-p (set value)
  (nth-value 1 (gethash value (slot-value set 'data))))

(defun set-empty-p (set)
  (zerop (hash-table-count (slot-value set 'data))))

(defun set-add (set value)
  (setf (gethash value (slot-value set 'data)) t)
  value)

(defun set-add-all (set seq)
  (map nil (curry #'set-add set) seq))

(defun set-remove (set value)
  (remhash value (slot-value set 'data))
  value)

(defun set-remove-all (set seq)
  (map nil (curry #'set-remove set) seq))

(defun set-clear (set)
  (clrhash (slot-value set 'data))
  set)

(defun set-random (set)
  (if (set-empty-p set)
    (values nil nil)
    (loop :with data = (slot-value set 'data)
          :with target = (random (hash-table-count data))
          :for i :from 0
          :for k :being :the :hash-keys :of data
          :when (= i target)
          :do (return (values k t)))))

(defun set-pop (set)
  (multiple-value-bind (val found) (set-random set)
    (if found
      (progn
        (set-remove set val)
        (values val t))
      (values nil nil))))


(defmethod print-object ((set hash-set) stream)
  (print-unreadable-object (set stream :type t)
    (format stream "~{~S~^ ~}"
            (iterate
              (for (key) :in-hashtable (slot-value set 'data))
              (collect key)))))


;;;; Iterate
(defmacro-clause (AVERAGING expr &optional INTO var)
  (with-gensyms (count)
    (let ((average (or var (gensym "average"))))
      `(progn
        (for ,average
             :first ,expr
             ;; continuously recompute the running average instead of keeping
             ;; a running total to avoid bignums when possible
             :then (/ (+ (* ,average ,count)
                         ,expr)
                      (1+ ,count)))
        (for ,count :from 1)
        ,(when (null var)
           ;; todo handle this better
           `(finally (return ,average)))))))

(defmacro-clause (TIMING time-type &optional SINCE-START-INTO var PER-ITERATION-INTO per)
  (let ((timing-function (ecase time-type
                           ((real-time) #'get-internal-real-time)
                           ((run-time) #'get-internal-run-time)))
        (since (or var (gensym))))
    (with-gensyms (start-time current-time previous-time)
      `(progn
        (with ,start-time = (funcall ,timing-function))
        (for ,current-time = (funcall ,timing-function))
        (for ,previous-time :previous ,current-time :initially ,start-time)
        (for ,since = (- ,current-time ,start-time))
        ,(when per
           `(for ,per = (- ,current-time ,previous-time)))
        ,(when (and (null var) (null per))
           `(finally (return ,since)))))))

(defmacro-driver (FOR var IN-WHATEVER seq)
  "Iterate over items in the given sequence.

  Unlike iterate's own `in-sequence` this won't use the horrifically inefficient
  `elt`/`length` functions on a list.

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (is-list source i len)
      `(progn
        (with ,source = ,seq)
        (with ,is-list = (typep ,source 'list))
        (with ,len = (if ,is-list -1 (length ,source)))
        (for ,i :from 0)
        (,kwd ,var next (if ,is-list
                          (if ,source
                            (pop ,source)
                            (terminate))
                          (if (< ,i ,len)
                            (elt ,source ,i)
                            (terminate))))))))
