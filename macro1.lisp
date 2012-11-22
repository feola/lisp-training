;; Сделать простой макрос

(defmacro test (x)
  (cons '+ x))

(test (2 3 4))

;; Сделать макрос, который создаёт функцию

(defmacro macrosumma (x y)
  '(defun summa (x y) (+ x y)))

(macrosumma x y)

(macroexpand-1 '(macrosumma x y))

;; Сделать макрос, который создаёт функцию с любым именем

(defmacro
