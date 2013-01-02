;;---------------------------------------------------
;; ПОСТРОЕНИЕ СПИСКОВ
;;---------------------------------------------------
;; copy-list - принимает список и возвращает его копию

(setf x '(a b c)
      y (copy-list x))

;; => (A B C)

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

;; => OUR-COPY-LIST

;; Можно определить функцию our-copy-list
;; Если список - это атом, вывести список
;; Иначе объединить голову списка и результат
;; вызова функции our-copy-list для хвоста
;; списка (рекурсия)


(our-copy-list '(a b (c d)))

;; => (A B (C D))

;;---------------------------------------------------
;; append - склеивает между собой произвольное количество списков

(append '(a b) '(c d) '(e))

;; => (A B C D E)

(append '(1 2 3) '(4 (5 6)))

;; => (1 2 3 4 (5 6))

;;---------------------------------------------------
;;ПРИМЕР - СЖАТИЕ
;;---------------------------------------------------
;; Можно определить функцию compress, которая принимает
;; список из атомов и возвращает его сжатое представление

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

;; => COMPRESS

;; Если х - cons-ячейка, применить функцию compr для головы списка
;; (с 1-м повторением) и для остатка (головы списка)
;; Иначе - вывести х

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

;; => COMPR

;; У функции 3 аргумента - последний элемент, число его повторений и
;; остаток списка
;; Если список пустой - применяется функция n-elts
;; Иначе задаётся локальная переменная next для головы списка
;; И если next равна последнему элементу, вызывается compr с
;; количеством повторений n+1 для хвоста списка
;; Иначе производится объединение в cons-ячейку (n-elts elt n)
;; и (compr next 1 (cdr lst))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

;; => N-ELTS

;; Если количество повторений элемента больше 1, составляется список
;; из количества повторений и элемента
;; Иначе выводится значение элемента

(compress '(1 1 1 0 1 0 0 0 0 1))

;; => ((3 1) 0 1 (4 0) 1)

(compress '(1 1 2 2 3 3 3 0 2 2 4))

;; => ((2 1) (2 2) (3 3) 0 (2 2) 4)

;; Чтобы реконструировать полученный список, можно определить
;; функцию uncompress

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

;; => UNCOMPRESS

;; Если список пуст - вывести nil
;; Иначе задаётся локальная переменная elt, равная хвосту списка
;; и соответствующая остатку применения функции uncompress для
;; хвоста списка
;; Если elt - это cons-ячейка, "склеить" список с  применением
;; функцию list-of (она раскрывает списки) и остаток
;; Иначе "склеить" элемент и остаток

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

;; => LIST-OF

;; Если количество повторений равно 0, вывести nil, иначе
;; объединить в ячейку элемент и применить list-of к количеству
;; повторений элемента n-1 и элементу


(uncompress '((3 1) 0 1 (4 0) 1))

;; => (1 1 1 0 1 0 0 0 0 1)

(uncompress '((2 1) (2 2) (3 3) 0 (2 2) 4))

;; => (1 1 2 2 3 3 3 0 2 2 4)

