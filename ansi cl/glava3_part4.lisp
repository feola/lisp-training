;;---------------------------------------------------
;; МНОЖЕСТВА
;;---------------------------------------------------
;; member - проверяет, принадлежит ли элемент множеству,
;; задаваемому списком
;; Возвращает не T, а часть списка, начиная с найденного элемента

(member 'b '(a b c))

;; => (B C)

(member 5 '(3 6 7 5 8 1))

;; => (5 8 1)

(member 'x '(y z))

;; => NIL

;; :test - аргумент по ключу, позволяющий сравнивать элементы
;; с помощью equal

(member '(a) '((a) (z)) :test #'equal)

;; => ((A) (Z))

;; :key - аргумент по ключу, с помощью которого можно задать
;; функцию, применяемую к каждому аргументу перед сравнением

(member 'a '((a b) (c d)) :key #'car)

;; => ((A B) (C D))

(member 2 '((1) (2)) :key #'car :test #'equal)

;; => ((2))

;;---------------------------------------------------
;; member-if - находит элемент, удовлетворяющий произвольному
;; предикату

(member-if #'oddp '(2 3 4))

;; => (3 4)

; oddp - прверяет, является ли арумент нечётным

;; Можно определить свою функцию  member-if

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst)
                    lst
                    (our-member-if fn (cdr lst))))))

;; => OUR-MEMBER-IF

;; Функция принимает 2 аргумента
;; fn - прозвольный предикат (применяемая функция)
;; lst - список
;; Сначала проверяется, является ли список cons-ячейкой
;; Если fn применима к голове списка, вывести список
;; Иначе выполнить our-member-if к хвосту списка

;;---------------------------------------------------
;; adjoin - присоединяет заданный элемент к списку, если его
;; нет в этом списке (условный cons)

(adjoin 'b '(a b c))

;; => (A B C)
;; Не прибавил, так как b есть в списке

(adjoin 'z '(a b c))

;; => (Z A B C)

;;---------------------------------------------------
;; union - функция объединения множеств

(union '(a b c) '(c b s))

;; => (A C B S)
;; Элемент b есть во втором списке, поэтому он пропускается

;;---------------------------------------------------
;; intersection - функция пересечения множеств

(intersection '(a b c) '(b b c))

;; => (C B)

;;---------------------------------------------------
;; set-difference - функция дополнения множеств

(set-difference '(a b c d e) '(b e))

;; => (D C A)

;; Перечисленные функции не сохраняют при выводе результата
;; порядок элементов в исходном списке
;; Эти функции могут иметь те же аргументы по ключу, что и member

;;---------------------------------------------------
;; ПОСЛЕДОВАТЕЛЬНОСТИ
;;---------------------------------------------------
;; length - определяет длину последовательности (списка)

(length '(a b c))

;; => 3

;;---------------------------------------------------
;; subseq - копирует часть последовательности
;; 1-ый аргумент - последовательность
;; 2-ой аргумент (обязательный) - задаёт начало последовательности
;; 3-ий арумент (необязательный) - индекс первого элемента, не
;; подлежащего копированию

(subseq '(a b c d) 1 2)

;; => (B)

(subseq '(a b c d) 1)

;; => (B C D)

;;---------------------------------------------------
;; reverse - возвращает последовательность, содержащую исходный элементы
;; в обратном порядке

(reverse '(a b c))

;; => (C B A)

(reverse '(0 1 2 3 4 5))

;; => (5 4 3 2 1 0)

;;---------------------------------------------------
;; Палиндромы - последовательности, читаемые одинаково в прямом
;; и обратном порядке

;; Можно определить функцию mirror?, которая выявит, является ли
;; список палиндромом

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))

;; => MIRROR?

;; Сначала задаётся локальная переменная len, которая равна
;; количеству элементов последовательности
;; Последовательность с нечётным количеством элементов не может быть
;; палиндромом, в этом случае возвращается nil
;; Если len - чётное, задаётся локальная переменная mid, равная
;; половине len
;; Потом копируется часть последовательности от самого начала до mid
;; И сравнивается с частью последовательности до mid в обратном порядке
;; Если они равны, выводится T, то есть последовательность - палиндром
;; Иначе возвращается nil

(mirror? '(a b b a))

;; => T

(mirror? '(1 2 3 4))

;; => NIL

;; Есть более простой способ проверки палиндрома

(defun mirror?-ol (x)
  (equal x (reverse x)))

;; => MIRROR?-OL

(mirror?-ol '(b c c b))

;; => T

;;---------------------------------------------------
;; sort - сортирует последовательность
;; Принимает 2 аргумента - список и функцию сравнения
;; Внимание! sort является деструктивной функцией, то есть
;; меняет исходный список

(sort '(0 2 1 3 8) #'>)

;; => (8 3 2 1 0)

(sort '(0 2 1 3 8) #'<)

;; => (0 1 2 3 8)

;; Можно определить функцию, которая принимает целое число n и
;; возвращает n-ый элемент в порядке убывания

(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

;; => NTHMOST

;; Возвращается указанный элемент (по пордку, начиная с 1) для
;; отсортированной копии последовательности (списка) по убыванию

(nthmost 2 '(0 2 1 3 8))

;; => 3

(nthmost 5 '(5 0 3 34 100))

;; => 0


;;---------------------------------------------------
;; every (каждый) и some (некоторые) - применяют предикат к одной
;; или нескольким последовательностям
;; Если передана только одна последовательность, выполняется проверка -
;; удовлетворяет ли каждый её элемен предикату

(every #'oddp '(1 3 5))

;; => T
;; Все нечётные

(some #'evenp '(1 2 3))

;; => T
;; Есть чётное - 2

(every #'> '(1 3 5) '(0 2 4))

;; => T
;; Все элементы соответствуют предикату




