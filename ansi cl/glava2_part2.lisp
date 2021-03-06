;;---------------------------------------------------
;; defun - определяет новые функции

(defun our-third (x)
  (car (cdr (cdr x))))

;; => OUR-THIRD

(our-third '(7 2 6 3 1))

;; => 6

(defun sum-greater (x y z)
  (> (+ x y) z))

;; => SUM-GREATER

(sum-greater 2 3 1)

;; => T

;;---------------------------------------------------
;; Рекурсивные функции - вызывают сами себя

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

;; => OUR-MEMBER

;; Прверяет, есть ли в списке данный объект
;; Если список пустой - вывести nil, иначе проверить -
;; если голова списка равна объекту, вывести список, а если
;; нет - повторить действия для оставшеося хвоста списка

(our-member 'd '(a b c d))

;; => (D)

(our-member 'Олеся '(Катя Таня Олеся))

;; => (ОЛЕСЯ)

(our-member 'лето '(зима весна лето осень))

;; => (ЛЕТО ОСЕНЬ)

;;---------------------------------------------------
;; format - основная функция вывода
;; Первый аргумент определяет, куда будет напечатан результат
;; Второй аргумент - строковый шаблон
;; Третий и последующие аргументы - объекты, вставляемые в шаблон

(format t "~A plus ~A equals ~A~%" 2 3 (+ 2 3))

;; CL-USER> (format t "~A plus ~A equals ~A~%" 2 3 (+ 2 3))
;; 2 plus 3 equals 5
;; NIL

(format t "~A корочки ~A" (- 5 2) "хлеба")

;; CL-USER> (format t "~A корочки ~A" (- 5 2) "хлеба")
;; 3 корочки хлеба
;; NIL

;;---------------------------------------------------
;; read - стандартная функция чтения, выполняет чтение
;; из стандартного места (ждёт ввода)

(defun askem (string)
  (format t "~A" string)
  (read))

;; => ASKEM

(askem "Сколько тебе лет?")

;; CL-USER> 28
;; => 28

;;---------------------------------------------------
;; let - позволяет ввести новые локальные переменные

(let ((x 1) (y 2))
  (+ x y))

;; => 3

(defun ask-number ()
  (format t "Пожалуйста, введите число ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

;; => ASK-NUMBER
;; Переменная val содержит функцию read
;; Если значение var - число, то оно возвращается,
;; если нет - выполняется функция ask-number, т. е.
;; происходит печать указанной строки

;; CL-USER> (ask-number)
;; Пожалуйста, введите число d
;; Пожалуйста, введите число g
;; Пожалуйста, введите число "hhh"
;; Пожалуйста, введите число 3
;; 3

;;---------------------------------------------------
;; defparameter - создаёт глобальные переменные (действительные везде)

(defparameter *glob* 99)

;; => *GLOB*

*glob*

;; => 99

;;---------------------------------------------------
;; defconstant - создаёт константы в глобальном окружении

(defconstant +limit+ (+ *glob* 1))

;; => +LIMIT+

+limit+

;; => 100

;;---------------------------------------------------
;; boundp - проверяет, соответствует ли имя глобальной переменной
;; или константе

(boundp '*glob*)

;; => T

(boundp '+limit+)

;; => T

(boundp 'a)

;; => NIL

;;---------------------------------------------------
;; setf - оператор, присваивающий значения переменным
;; любых типов

(setf *glob* 98)

;; => 98

(let ((n 10))
  (setf n 2)
  n)

;; => 2

(setf x (list 'a 'b 'c))

;; => (A B C)

x

;; => (A B C)

(setf (car x) 'n)

;; => N

x

;; => (N B C)






