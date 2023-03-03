(unless (find-package :silt)
  (ql:quickload '(:silt) :silent t))

(declaim (optimize (debug 1) (safety 1) (speed 3)))

(let ((*standard-output* (make-broadcast-stream)) ; shut
      (*error-output* (make-broadcast-stream))) ; up
  (asdf:load-system 'silt :force t))

(defun main (&rest argv)
  (declare (ignore argv))
  (silt::main))

(sb-ext:save-lisp-and-die "silt"
                          :toplevel 'silt::run
                          :save-runtime-options t
                          :executable t)
