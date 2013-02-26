(require 'bordeaux-threads)
(use-package (find-package 'bordeaux-threads))

(defparameter *so* *standard-output*)
(defparameter *i* 0)
(defparameter *lock* (make-lock "my-lock"))

(defun thread-a ()
    (do ((n))
              ((> *i* 1000000) (print "a wins!" *so*))
          (my-print "a")
          (incf *i*)))

(defun thread-b ()
    (do ((n))
              ((< *i* -1000000) (print "b wins!" *so*))
          (my-print "b")
          (decf *i*)))

(defun my-print (name)
    (acquire-lock *lock* t)
      (format *so* "~%~A: ~A" name *i*)
        (release-lock *lock*))

(progn
    (make-thread #'thread-b :name "b")
      (make-thread #'thread-a :name "a"))

(all-threads)
