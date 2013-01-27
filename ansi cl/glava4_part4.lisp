;;---------------------------------------------------
;; Структура - это продвинутый вариант вектора

(defun block-height (b) (svref b 0))

;; => BLOCK-HEIGHT

;; defstruct - определяет структуру

(defstruct point
  x
  y)

;; => POINT

;; Приопределении структуры неявно задаются функции
;; make-point, point-p, copy-point, point-x, point-y

;; make-point - при каждом вызове возвращает вновь созданный экземпляр
;; структуры point

(setf p (make-point :x 0 :y 0))

;; => #S(POINT :X 0 :Y 0)

;; point-x, point-y - функции доступа к полям

(point-x p)

;; => 0

(setf (point-y p) 2)

;; => 2

p

;; => #S(POINT :X 0 :Y 2)

;; point-p - это проверка типа

(point-p p)

;; => T

(typep p 'point)

;; => T

;; Можно задать значения полей по умолчанию

(defstruct polemic
  (type (progn
          (format t "What kind of polemic was it? ")
          (read)))
  (effect nil))

;; => POLEMIC

;; CL-USER> (make-polemic)
;; What kind of polemic was it? scathing
;; #S(POLEMIC :TYPE SCATHING :EFFECT NIL)

;; Более развитый вариант определения структуры point:
;; Здесь можно управлять способом отображения структуры и
;; префиксом имён функций для доступа к полям

(defstruct (point (:conc-name p)
                  (:print-function print-point))
  (x 0)
  (y 0))

;; => POINT

(defun print-point (p stream depth)
  (format stream "#<~A, ~A>" (px p) (py p)))

;; => PRINT-POINT

;; :conc-name - задаёт префикс, с которого будут начинаться имена
;; функций для доступа к полям структуры (по умолчанию point)
;; :print-function - это имя функции, которая будет вызываться
;; для печати объекта, когда его нужно будет отобразить

(make-point)

;; => #<0, 0>

;;---------------------------------------------------
;; ПРИМЕР - ДВОИЧНЫЕ ДЕРЕВЬЯ ПОИСКА
;;---------------------------------------------------
;; Можно рассмотреть метод хранения объектов в двоичном дереве
;; поиска - BST
;; В нашем случае BST - это бинарное дерево, в котором для каждого элемента и
;; функции < соблюдается правило: левый дочерний элемент < родителя, правый >

(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

;; => NODE

;; Здесь задаётся структура - указывается функция печати для узла,
;; поле самого узла и его потомков - левого и правого

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l (bst-insert obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-insert obj (node-r bst) <)
                 :l (node-l bst)))))))

;; => BST-INSERT

;; Функция bst-insert позволяет построить дерево
;; Она принимает 3 аргумента - объект (список элементов), дерево и
;; функцию упорядочения

(setf nums nil)

;; => NIL

(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf nums (bst-insert x nums #'<)))

;; => NIL

;; Теперь создано соответствующее дерево

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

;; => BST-FIND

;; Функция bst-find ищет объекты в дереве, она принимает те же аргументы,
;; что bst-insert
;; Возвращает не сам элемент, а его поддерево

(bst-find 12 nums #'<)

;; => NIL
;; Т. к. числа 12 в дереве нет

(bst-find 4 nums #'<)

;; => #<4>

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

;; => BST-MIN

;; Функция bst-min позволяет найти минимальный элемент дерева
;; Чтобы найти минимальный элемент, нужно идти по дереву, всегда выбирая
;; левую ветвь

(bst-min nums)

;; => #<1>

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

;; => BST-MAX

;; Функция bst-min позволяет найти максимальный элемент дерева
;; Чтобы найти максимальный элемент, нужно идти по дереву, всегда выбирая
;; правую ветвь

(bst-max nums)

;; => #<9>

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l (bst-remove obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-remove obj (node-r bst) <)
                 :l (node-l bst)))))))

;; => BST-REMOVE

;; Функция bst-remove позволяет удалить элемент из бинарного дерева
;; Она принимает объект, дерево и функцию упорядочения и возвращает
;; это же дерево без заданного элемента

(setf nums (bst-remove 2 nums #'<))

;; => #<5>

(bst-find 2 nums #'<)

;; => NIL
;; Всё, цифры 2 уже нет в дереве

(defun percolate (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t (if (zerop (random 2))
                 (make-node :elt (node-elt (bst-max l))
                            :r r
                            :l (bst-remove-max l))
                 (make-node :elt (node-elt (bst-min r))
                            :r (bst-remove-min r)
                            :l l))))))

;; => PERCOLATE

;; Функция percolate позволяет заместить удалённый элемент дерева одним
;; из его поддеревьев

(defun bst-remove-min (bst)
  (if (null (node-l bst))
      (node-r bst)
      (make-node :elt (node-elt bst)
                 :l (bst-remove-min (node-l bst))
                 :r (node-r bst))))

;; => BST-REMOVE-MIN

;; Удаляет минимальный элемент

(defun bst-remove-max (bst)
  (if (null (node-r bst))
      (node-l bst)
      (make-node :elt (node-elt bst)
                 :l (node-l bst)
                 :r (bst-remove-max (node-r bst)))))

;; => BST-REMOVE-MAX

;; Удаляет максимальный элемент

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

;; => BST-TRAVERSE

;; Функция bst-traverse обеспечивает последовательный обход элементов дерева

;; CL-USER> (bst-traverse #'princ nums)
;; 13456789
;; NIL

