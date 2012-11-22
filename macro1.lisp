;; Сделать макрос, который создаёт функцию.

(defmacro macrosumma (x y)
  '(defun summa (x y) (+ x y)))

(macrosumma x y)

(macroexpand-1 '(macrosumma x y))
