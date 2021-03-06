;;---------------------------------------------------
;; УПРАЖНЕНИЯ
;;---------------------------------------------------

;;---------------------------------------------------
;; 1. Представьте следующие списки в виде ячеек

(a b (c d))

;; |---|---|   |---|---|   |---|---|
;; | a |   | > | b |   | > |   |nil|
;; |---|---|   |---|---|   |---|---|
;;                           |
;;                           |
;;                         |---|---|   |---|---|
;;                         | c |   | > | d |nil|
;;                         |---|---|   |---|---|

(a (b (c (d))))

;; |---|---|   |---|---|
;; | a |   | > |   |nil|
;; |---|---|   |---|---|
;;               |
;;               |
;;             |---|---|   |---|---|
;;             | b |   | > |   |nil|
;;             |---|---|   |---|---|
;;                           |
;;                           |
;;                         |---|---|   |---|---|
;;                         | c |   | > |   |nil|
;;                         |---|---|   |---|---|
;;                                       |
;;                                       |
;;                                     |---|---|
;;                                     | d |nil|
;;                                     |---|---|

(((a b) c) d)

;; |---|---|   |---|---|
;; |   |   | > | d |nil|
;; |---|---|   |---|---|
;;   |
;;   |
;; |---|---|   |---|---|
;; |   |   | > | c |nil|
;; |---|---|   |---|---|
;;   |
;;   |
;; |---|---|   |---|---|
;; | a |   | > | b |nil|
;; |---|---|   |---|---|

(a (b . c) . d)

;; |---|---|---|
;; | a |   | d |
;; |---|---|---|
;;       |
;;       |
;;     |---|---|
;;     | b | c |
;;     |---|---|

;;---------------------------------------------------
;; 2. Напишите свой вариант функции union, который сохраняет порядок следования
;; элементов согласно исходным спискам

(defun my-union (a b)
  (if (null b)
      a
      (my-union (reverse (adjoin (car b) (reverse a)))
                (cdr b))))

(my-union '(a f c) '(b c d))

;; => (A F C B D)

(my-union '(a b c) '(d c e a f))

;; => (A B C D E F)

(my-union '(3 4 5) '(1 2 3 4 6))

;; => (3 4 5 1 2 6)

;;---------------------------------------------------
;; 3. Напишите функцию, определяющую количество повторений (с точки зрения eql)
;; каждого элемента в заданном списке и сортирующую их по убыванию встречаемости

;; Первый вариант (мой):

(defun occurrences (lst)
  (sort
   (remove nil
           (cons
            (if (not (null lst))
                (cons (car lst) (find-rec (car lst) lst)))
            (if (not (null (cdr lst)))
                (occurrences (remove (car lst) lst)))))
    #'(lambda (x y) (> (cdr x) (cdr y)))))

;; => OCCURRENCES

(defun find-rec (term lst)
  (if (null lst)
      0
      (if (eql term (car lst))
          (+ (find-rec term (cdr lst)) 1)
          (find-rec term (cdr lst)))))

;; => FIND-REC

(occurrences '(a c a b b a))

;; => ((A . 3) (B . 2) (C . 1))

;; Второй вариант (rigidus):

(let ((x))
(let ((over nil)
      (isset nil))
  (defun reclist (lst)
    (if (null over)
        (setf over lst))
    (if (null lst)
        (sort x #'(lambda (a b) (> (cdr a) (cdr b))))
        (progn
          (if (not (find (car lst) isset))
              (progn
                (push
                 (cons
                  (car lst)
                  (find-rec (car lst) over)) x)
                (push (car lst) isset)))
          (reclist (cdr lst)))))))

;; => RECLIST

(defun find-rec (term lst)
  (if (null lst)
      0
      (if (eql term (car lst))
          (+ (find-rec term (cdr lst)) 1)
          (find-rec term (cdr lst)))))

;; => FIND-REC

(reclist '(a f a a t a t f f f f f a))

;; => ((F . 6) (A . 5) (T . 2))

;;---------------------------------------------------
;; 4. Почему (member '(a) '((a) (b))) возвращает nil?

(member '(a) '((a) (b)))

;; => NIL

;; Потому что с точки зрения eql (a) и (a) не равны, они соответствуют
;; в памяти лиспа разным объектам

(eql '(a) '(a))

;; => NIL

;; Чтобы выполнить сравнение, необходимо добавить ключ

(member '(a) '((a) (b)) :test #'equal)

;; => ((A) (B))

;;---------------------------------------------------
;; 5. Функция pos+ принимает список и возвращает новый, каждый элемент
;; которого увеличен по сравнению с исходным на его положение в списке
;; Определите функцию с помощью (a) рекурсии, (b) итерации и (c) mapcar

;; a - рекурсия

(defun a-pos+ (lst)
  (if (not (null lst))
      (reverse (cons (+ (car lst) (length (reverse (cdr lst))))
               (reverse (a-pos+ (cdr lst)))))
        lst))

(a-pos+ '(1 1 1 1))

;; => (1 2 3 4)

;; b - итерация

(defun b-pos+ (lst)
  (let (x)
    (dolist (obj lst x)
      (setf x (cons (+ obj (length x)) x)))
    (reverse x)))

(b-pos+ '(1 1 1 1))

;; => (1 2 3 4)

;; c - mapcar

(defun c-pos+ (lst)
  (mapcar #'+ '(0 1 2 3) lst))

(c-pos+ '(1 1 1 1))

;; => (1 2 3 4)

;; Работает только для списка с ограниченным количеством элементов
;; Возможно ли сделать функцию для списка с любым количеством элементов?

;; Первый вариант (мой):

(let ((x 0))
  (let ((n-lst nil))
    (defun c-pos+ (lst)
      (if (null lst)
          n-lst
          (progn
            (setf x (+ x 1))
            (setf n-lst (push (- x 1) n-lst))
            (c-pos+ (cdr lst))))
      (mapcar #'+ (reverse n-lst) lst))))

(c-pos+ '(1 1 1 1))

;; => (1 2 3 4)

;; Второй вариант (rigidus):

(defun foo (p)
  (let ((acc))
    (do ((x (length p)))
        ((< x 0))
      (push x acc)
      (decf x))
    (mapcar #'+ acc p)))

(foo '(1 1 1 1))

;; => (1 2 3 4)

;;---------------------------------------------------
;; 6. Определить функции cons, length и member, считая, что теперь
;; cdr указывает на голову списка, а car - на хвост

(defun my-cons (x y)
  (let ((a '(x . y)))
    (setf (cdr a) x)
    (setf (car a) y)
    a))

(my-cons 'a 'b)

;; => (B . A)

;; Следующие функции определены без замены car на cdr и наоборот
;; Оставляю это в таком виде

(let ((x 0))
  (defun my-length (lst)
    (if (null lst)
        x
        (progn
          (setf x (+ x 1))
        (my-length (cdr lst))))))

(my-length '(s d f g))

;; => 4

(defun my-member (elt lst)
    (if (eql (car lst) elt)
        (cons elt (cdr lst))
        (my-member elt (cdr lst))))

(my-member 'n '(r j n e q))

;; => (N E Q)

;;---------------------------------------------------
;; 7. Измените программу так, чтобы она создавала меньшее количество ячеек

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

;; Это программа для "сжатия", которая разбиралась в 3 главе

(compress '(1 1 1 0 1 0 0 0 0 1))

;; => ((3 1) 0 1 (4 0) 1)

;; Надо переделать таким образом, чтобы в результате получились не
;; правильные списки, а точечные пары

(defun my-compress (x)
  (if (consp x)
      (my-compr (car x) 1 (cdr x))
      x))

(defun my-compr (elt n lst)
  (if (null lst)
      (list (my-n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (my-compr elt (+ n 1) (cdr lst))
            (cons
             (if (> n 1)
                 (my-n-elts (list elt) n)
                 (my-n-elts elt n))
                  (my-compr next 1 (cdr lst)))))))

(defun my-n-elts (elt n)
  (if (> n 1)
      (cons n (car elt))
      elt))

;; Я изменила следующее:
;; 1. В определении my-n-elts строку (list n elt) заменила на (cons n (car elt)),
;; для того, чтобы при повторении элементов создавалась точечная пара
;; 2. В том случае, если элемент далее не повторяется, для применения функции
;; my-n-elts неоходимо, чтобы elt был списком, поэтому я определила эту
;; часть как (my-n-elts (list elt) n)
;; 3. Чтобы в окончательном результате для эелементов, повторяющихся один раз
;; не создавался список, ввожу условие - (> n 1), если оно не соблюдается,
;; получаем (my-n-elts elt n), в отличие от строки в п. 2

(my-compress '(1 1 1 0 1 0 0 0 0 1))

;; => ((3 . 1) 0 1 (4 . 0) 1)

;;---------------------------------------------------
;; 8. Определите функцию, печатающую заданный список в точечной нотации

(defun showdots (lst)
    (if (null lst)
        nil
        (cons (car lst)
              (cons (showdots (cdr lst)) nil))))

(showdots '(a b c))

;; => (A (B (C NIL)))

;; Получилось сделать список требуемого вида, но его надо переделать
;; в точечную нотацию

(defun showdots (lst)
  (format t "(~A . (~A . (~A . NIL)))"
          (car lst)
          (car (cdr lst))
          (car (cdr (cdr lst)))))

(showdots '(a b c))

;; CL-USER> (showdots '(a b c))
;; (A . (B . (C . NIL)))
;; NIL

;; Упражнение выполнено, но остался вопрос - как сделать, чтобы функция
;; работала с любым количеством аргументов?

(defun showdots (lst)
  (if (not (null (cdr lst)))
      (progn
        (format t "(~A . " (car lst))
        (showdots (cdr lst))
        (do ((i (length lst) (- i 1)))
            ((= i 0)
             (format t ")"))))
      (format t "(~A . NIL)" (car lst))))

(showdots '(a b c d))

;; (A . (B . (C . (D . NIL))))

(defun showdots (lst)
  (if (not (null (cdr lst)))
      (progn
        (format t "(~A . " (car lst))
        (showdots (cdr lst))
        (format t ")"))
      (format t "(~A . NIL)" (car lst))))

(showdots '(a b c d))

;; (A . (B . (C . (D . NIL))))

;;---------------------------------------------------
;; 9. Напишите программу, которая ищет наиболее длинный путь в сети, не содержащий
;; повторений. Сеть может содержать циклы

(setf my-net-one '((a b c d) (b c) (c d)))

(setf my-net-two '((a b c) (b c) (c d e) (d e)))

(setf my-net-cycle '((a b c d) (b c) (c d) (d a)))

(defun longest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (append (reverse (car (cdr queue))) (list end))
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
            (cdr (assoc node net))))

(longest-path 'a 'd my-net-one)

;; => (A B C D)

(longest-path 'a 'e my-net-two)

;; => (A B C D E)

(longest-path 'a 'd my-net-cycle)

;; => (A B C D)
