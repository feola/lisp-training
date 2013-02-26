(require 'bordeaux-threads)
(use-package (find-package 'bordeaux-threads))

(defparameter *so* *standard-output*)
(defparameter *i* 0)
(defparameter *lock* (make-lock "my-lock"))

(defmacro with-lock (name)
  `(defun my-print (name)
     (acquire-lock *lock* t)
     (format *so* "~%~A: ~A" ,name *i*)
     (release-lock *lock*)))

(with-lock "a")
;; Раскрывается в
;; => (PROGN
;;      (EVAL-WHEN (:COMPILE-TOPLEVEL) (SB-C:%COMPILER-DEFUN 'MY-PRINT 'NIL T))
;;      (EVAL-WHEN (:LOAD-TOPLEVEL :EXECUTE)
;;        (SB-IMPL::%DEFUN 'MY-PRINT
;;                         (SB-INT:NAMED-LAMBDA MY-PRINT
;;                             (NAME)
;;                           (BLOCK MY-PRINT
;;                             (ACQUIRE-LOCK *LOCK* T)
;;                             (FORMAT *SO* "~%~A: ~A" "a" *I*)
;;                             (RELEASE-LOCK *LOCK*)))
;;                         NIL 'NIL (SB-C:SOURCE-LOCATION)))), T

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

(progn
  (make-thread #'thread-b :name "b")
  (make-thread #'thread-a :name "a"))

(all-threads)
