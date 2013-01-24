;;---------------------------------------------------
;; elt - позволяет получить доступ к элементу последовательности
;; любого типа

(elt '(a b c) 1)

;; => B

(elt "string" 4)

;; => #\n

;; elt рекомендуется использовать только тогда, когда тип последовательности
;; заранее не известен

;;---------------------------------------------------
;; Новое определение функции mirror? (рассматривалась ранее) для векторов
;; Проверяет, является ли последовательность палиндромом

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)))
             ((or (> forward back)
                  (not (eql (elt s forward)
                            (elt s back))))
              (> forward back))))))

;; => MIRROR?

(mirror? "abccba")

;; => T

(mirror? #(1 2 2 1))

;; => T

(mirror? #(1 2 3 4))

;; => NIL

;;---------------------------------------------------
;; Аргументы по ключу для функций, работающих с последовательностями:

;; :key - функция, применяемая к каждому аргументу (по умолчанию identity)
;; :test - предикат для сравнения (по умолчанию eql)
;; :from-end - если Т, работать с конци (по умолчанию nil)
;; :start - индекс элемента, с которого начинается выполнение (по умолчанию 0)
;; :end - индекс элемента, на котором следует остановиться (по умолчанию nil)

;;---------------------------------------------------
;; position - возвращает положение определённого элемента в последовательности
;; или nil в случае его отсутствия

(position #\a "fantasia")

;; => 1

(position #\a "fantasia" :start 3 :end 5)

;; => 4

(position #\a "fantasia" :from-end t)

;; => 7

(position 'a '((c d) (a b)) :key #'car)

;; => 1

(position '(a b) '((a b) (c d)))

;; => NIL

(position '(a b) '((a b) (c d)) :test #'equal)

;; => 0

(position 3 '(1 0 7 5) :test #'<)

;; => 2

;;---------------------------------------------------
;; subseq и position позволяют разделить последовательность на части

(defun second-word (str)
  (let ((p1 (+ (position #\  str) 1)))
    (subseq str p1 (position #\  str :start p1))))

;; => SECOND-WORD

(second-word "Form follows function.")

;; => "follows"

;;---------------------------------------------------
;; position-if - выполняет поиск элементов, удовлетворяющих заданному предикату

(position-if #'oddp '(2 3 4 5))

;; => 1

;;---------------------------------------------------
;; find - позволяет найти элемент в последовательности

(find #\a "cat")

;; => #\a

(find-if #'characterp "ham")

;; => #\h

;;---------------------------------------------------
;; remove-duplicates - удаляет все повторяющиеся элементы последовательности,
;; кроме последнего

(remove-duplicates "abracadabra")

;; => "cdbra"

;;---------------------------------------------------
;; reduce - сводит последовательность в одно значение

(reduce #'intersection '((b r a d 's) (b a d) (c a t)))

;; => (A)

;; В этом примере reduce используется для пересечения списков

;;---------------------------------------------------
;; ПРИМЕР - РАЗБОР ДАТ
;;---------------------------------------------------
;; Прграмма должна превращать строку типа "16 Aug 1980" в целые числа,
;; соответствующие дню, месяцу и году

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

;; => TOKENS

;; Эта функция выделяет знаки из строки
;; tokens принимает строку и предикат, и возвращает список подстрок,
;; удовлетворяющих предикату

(tokens "ab12 3cde.f" #'alpha-char-p 0)

;; => ("ab" "cde" "f")

;; В данном случае alpha-char-p - это предикат, удовлетворяющий буквенным знакам

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

;; => CONSTITUENT

;; Данная функция будет использоваться в качестве предиката для tokens
;; graphic-char-p - предикат, удовлетворяющий печатным знакам, к которым
;; также относится и пробел

(tokens "ab12 3cde.fgh" #'constituent 0)

;; => ("ab12" "3cde.fgh")

;; В данном случае пробелы мы должны исключить, что делается в последнем выражении
;; при определении функции constituent

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

;; => PARSE-DATE

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

;; => MONTH-NAMES

(defun parse-month (str)
  (let ((p (position str month-names
                     :test #'string-equal)))
    (if p
        (+ p 1)
        nil)))

;; => PARSE-MONTH

(parse-date "16 Aug 1980")

;; => (16 8 1980)

(parse-date "24 jan 2013")

;; => (24 1 2013)

;; Здесь используется встроенная функция parse-integer, позволяющая
;; преобразовать строку в число
;; Можно отдельно определить эту функцию

(defun read-integer (str)
  (if (every #'digit-char-p str)
      (let ((accum 0))
        (dotimes (pos (length str))
          (setf accum (+ (* accum 10)
                         (digit-char-p (char str pos)))))
        accum)
      nil))

;; => READ-INTEGER

;; digit-char-p - проверяет, является ли аргумент цифрой, и возвращает цифру,
;; если это так


(read-integer "34")

;; => 34

(read-integer "не цифра")

;; => NIL
