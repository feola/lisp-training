(defun hello-world () (format t "hello, world"))

(hello-world)

(defun verbose-sum (x y)
    "Sum any two numbers after printing a message."
      (format t "Summing ~d and ~d.~%" x y)
        (+ x y))

(verbose-sum 2 3)


