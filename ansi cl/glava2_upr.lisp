;;---------------------------------------------------
;; УПРАЖНЕНИЯ
;;---------------------------------------------------

;;---------------------------------------------------
;; 1. Опишите, что происходит при вычислении следующих выражений

(+ (- 5 1) (+ 3 7))

;; => 14

;; Сначала вычисляется список (- 5 1) 5-1=4
;; Затем вычисляется список (+ 3 7) 3+7=10
;; К полученным значениям применяется "+",
;; т. е. вычисляется (+ 4 10) 4+10=14
;; Выводится результат этого вычисления - 14

(list 1 (+ 2 3))

;; => (1 5)

;; 1-ый аргумент оператора list - это число, а второй
;; аргумент - список
;; Сначала вычисляется список (+ 2 3) 2+3=5
;; А затем из первого аргумента и второго вычисленного аргумента
;; составляется новый список, т. е. применяется (list 1 5)
;; Выводится результат - (1 5)

(if (listp 1) (+ 1 2) (+ 3 4))

;; => 7

;; Если 1 является списком - вывести результат вычисления (+ 1 2),
;; т. е. число 3
;; Иначе вывести результат вычисления (+ 3 4), т. е. число 7
;; Поскольку число 1 - это не список, выводится результат вычисления - 7

(list (and (listp 3) t) (+ 1 2))

;; => (NIL 3)

;; Первый аргумент list - список, содержащий выражение (and (listp 3) t)
;; Это значит, что если выражение (listp 3) является истинным, and вернёт
;; Т, а если ложным - nil
;; Поскольку число 3 не является списком, результатом вычисления будет nil
;; Второй аргумент - это выражение (+ 1 2), результат его вычислени равен 3
;; list применяется к полученным аргументам (list nil 3)
;; Выводится результат - (NIL 3)

;;---------------------------------------------------
;; 2. Составьте с помощью cons три различных выражения,
;; создающие список (a b c)

(cons 'a '(b c))

;; => (A B C)

(cons 'a (cons 'b (cons 'c nil)))

;; => (A B C)

(cons 'a (cons 'b '(c)))

;; => (A B C)

;;---------------------------------------------------
;; 3. С помощью car и cdr определите функцию, вызываэщую 4-ый
;; элемент списка

(defun chetv (sp)
  (car (cdr (cdr (cdr sp)))))

;; => CHETV

(chetv '(3 2 5 8 5 6))

;; => 8

(chetv '(первый второй третий четвёртый пятый))

;; => ЧЕТВЁРТЫЙ

;;---------------------------------------------------
;; 4. Определите функцию, принимающую 2 аргумента и возвращающую
;; наибольший

(defun big (x y)
  (if (> x y)
      x
      y))

;; => BIG

(big 8 11)

;; => 11

(big 43 28)

;; => 43

;;---------------------------------------------------
;; 5. Что делают следующие функции?

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

;; => ENIGMA

(enigma '(nil 4 5 8))

;; => T

;; Смысл функции - определить, есть ли в списке nil
;; Если есть - результат T, а если нет - nil

;; Если хотя бы одно из выражений, входящих в оператр end
;; будет ложным, результатом вызова функции будет nil
;; Сначала вычисляется (not (null x))
;; Если список не пустой (null x) - это nil, тогда (not nil) - это T
;; Если список пустой, то результат всего выражения (и самой функции) - nil
;; В том случае, если всё-таки список не пуст, вычисляется следующее выражение
;; Опратор or найдёт и выведет первое истинное значение
;; В данном случае сначала проверяется (null (car x))
;; Если голова списка - nil, значит выражение - истина, результат вызова функции - T
;; Если нет, переходим к следующему выражению (enigma (cdr x))
;; Оно вызывает ту же функцию, но для хвоста списка

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

;; => MYSTERY

(mystery 3 '(2 1 9 3 4 3))

;; => 3

(mystery 'b '(a b c))

;; => 1

;; Смысл функции - определить, какое количество объектов стоит перед
;; первым значением переменной х, если оно есть в списке y

;; У функции mystery 2 аргумента
;; Если y - это пустой список - вывести как результат вычисления nil
;; Иначе проверить: если голова списка у равна аргументу x, вывести 0
;; Если нет, задать локальную переменную z, которая вызывает функцию
;; mystery для аргумента x и хвоста y
;; Переменная будет вычисляться до тех пор, пока значение z не
;; окажется равным 0
;; Тогда за каждый "проход" по значениям из списка к z прибавится по 1,
;; эта сумма будет выведена как результат вычисления.

;;---------------------------------------------------
;; 6. Что может стоять на месте x в следующих выражениях

;; а. (car (x (cdr '(a (b c) d))))
;; B

;; Ответ - car

(car (car (cdr '(a (b c) d))))

;; => B


;; b. (x 13 (/ 1 0))
;; 13

;; Ответ - or

(or 13 (/ 1 0))

;; => 13

;; c. (x #'list 1 nil)
;; (1)

;; Ответ - apply

(apply #'list 1 nil)

;; => (1)

;;---------------------------------------------------
;; 7. Определите функцию, проверяющую, является ли списком хотя бы один
;; элемент списка

(defun there-is-list (lst)
  (if (listp (car lst))
      t
      (if (null (cdr lst))
          nil
          (there-is-list (cdr lst)))))

;; => THERE-IS-LIST

(there-is-list '(2 (1 2) 4 7))

;; => T

(there-is-list '(1 два "три"))

;; => NIL

;;---------------------------------------------------
;; 8. Предложите итеративное и рекурсивное определение
;; функции, которая:

;; а. печатает количество точек, которое равно заданному
;; положительному целому числу

(defun points (x)
  (if (not (typep x 'integer))
      (format t "это не целое положительное число")
      (do ((i x (- i 1)))
          ((= i 0) 'done)
        (format t "."))))

;; CL-USER> (points 7)
;; .......
;; DONE

;; CL-USER> (points 'a)
;; это не целое положительное число
;; NIL

(defun points-rec (x)
  (if (typep x 'integer)
      (if (> x 0)
          (progn
            (points-rec (- x 1))
            (format t ".")
            'done))
      (format t "это не целое положительное число")))

;; => POINTS-REC

;; CL-USER> (points-rec 4)
;; ....
;; DONE

;; CL-USER> (points-rec "cat")
;; это не целое положительное число
;; NIL

