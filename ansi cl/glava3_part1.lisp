;;---------------------------------------------------
;; ЯЧЕЙКИ
;;---------------------------------------------------
;; cons - объединяет 2 объекта в ячейку (это пара указателей,
;; 1-ый указывает на car, а 2-ой на cdr)
;; cons объединяет в пару объекты любых типов
;; Списки - это не отдельный вид объектов, а набор связанных между
;; собой cons-ячеек

(setf x (cons 'a nil))

;; => (A)

;; Получилась одна ячейка вида |---|---|
;;                             | a |nil|
;;                             |---|---|

(car x)

;; => A

(cdr x)

;; => NIL

(setf y (list 'a 'b 'c))

;; => (A B C)

;; |---|---|   |---|---|   |---|---|
;; | a |   | > | b |   | > | c |nil|
;; |---|---|   |---|---|   |---|---|

(cdr y)

;; => (B C)

(setf z (list 'a (list 'b 'c) 'd))

;; => (A (B C) D)

;; |---|---|   |---|---|   |---|---|
;; | a |   | > |   |   | > | d |nil|
;; |---|---|   |---|---|   |---|---|
;;               |
;;               |
;;             |---|---|   |---|---|
;;             | b |   | > | c |nil|
;;             |---|---|   |---|---|


(car (cdr z))

;; => (B C)

;;---------------------------------------------------
;; consp - проверяет, является ли объект cons-ячейкой

(consp '(a b))

;; => T

(consp nil)

;; => NIL

(defun out-listp (x)
  (or (null x) (consp x)))

;; => OUT-LISTP

;; Функция проверяет, является ли объект списком (аналог listp)
;; Чтобы значение функции было истинным, необходимо, чтобы выполнялось
;; одно из условий - либо обект пустой список, либо cons-ячейка

(out-listp 'не-список)

;; => NIL

(out-listp '(1 2 3))

;; => T

(out-listp nil)

;; => T

(defun our-atom (x) (not (consp x)))

;; => OUR-ATOM

;; Функция проверяет, является ли объект атомом
;; Чтобы значение функции было истинным, необходимо, чтобы
;; объект не был cons-ячейкой


(our-atom '(a b))

;; => NIL

(our-atom 'атом)

;; => T

;;---------------------------------------------------
;; РАВЕНСТВО
;;---------------------------------------------------
;; eql возвращает t только если сравниваемые значения соответствуют
;; одному объекту в памяти лиспа

(eql (cons 'a nil) (cons 'a nil))

;; => NIL

(setf x (cons 'a nil))

;; => (A)

(eql x x)

;; => T

;;---------------------------------------------------
;; equal - используется для проверки идентичности списков
;; (и других объектов)

(equal x (cons 'a nil))

;; => T

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

;; => OUR-EQUAL

;; Частный случай функции equal
;; Два арумента равны, если (eql x y) - истина
;; Или если истинны все перечисленные выражения
;; x - cons-ячейка
;; y - cons-ячейка
;; our-equal (car x) (car y) - это рекурсия
;; our-equal (cdr x) (cdr y) - это рекурсия

(our-equal '(1 2 3) '(1 2 3))

;; => T

(our-equal nil nil)

;; => T

(our-equal '(a b) '(b c))

;; => NIL

;;---------------------------------------------------
;; ПОЧЕМУ В ЛИСПЕ НЕТ УКАЗАТЕЛЕЙ
;;---------------------------------------------------
;; setf - при присваивании переменной соответствует указатель
;; на её значение

(setf x '(a b c))

;; => (A B C)

(setf y x)

;; => (A B C)

;; x -->   |---|---|   |---|---|   |---|---|
;;         | a |   | > | b |   | > | c |nil|
;; y -->   |---|---|   |---|---|   |---|---|

;; При присваивании место в памяти, связанное с переменной х, содержит
;; не  сам список, а указатель на него
;; Чтобы присвоить переменной у то е значение, достаточно просто скопировать
;; этот указатель

(eql x y)

;; => T

;; Указатели не используются, потому что любое значение, по сути,
;; является указателем

