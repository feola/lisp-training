;;---------------------------------------------------
;; make-hash-table - создаёт хеш-таблицу

(setf ht (make-hash-table))

;; => #<HASH-TABLE :TEST EQL :COUNT 0 {C0C67F9}>

;; gethash - позволяет получить значение, связанное с заданным ключом

(gethash 'color ht)

;; => NIL, NIL

;; Так как нет такого ключа и не найден элемент

(setf (gethash 'color ht) 'red)

;; => RED

(gethash 'color ht)

;; => RED, T

;;---------------------------------------------------
;; При желании сопоставить каждой функции её краткое описание
;; можно создать таблицу, в которой ключами будут функции, а
;; значениями - строки

(setf bugs (make-hash-table))

;; => #<HASH-TABLE :TEST EQL :COUNT 0 {BC245F9}>

(defun our-member (obj lst)
  (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst))))

(push "Doesn’t take keyword arguments. "
      (gethash #'our-member bugs))

;; => ("Doesn’t take keyword arguments. ")

;;---------------------------------------------------
;; Хеш-таблицы можно использовать вместо списков для представления множеств
;; Для добавления элемента в множество можно использовать setf вместе с gethash

(setf fruit (make-hash-table))

;; => #<HASH-TABLE :TEST EQL :COUNT 0 {BCFF151}>

(setf (gethash 'apricot fruit) t)

;; => T

(gethash 'apricot fruit)

;; => T, T

;;---------------------------------------------------
;; remhash - удаляет элемент из множества

(remhash 'apricot fruit)

;; => T

;;---------------------------------------------------
;; maphash - используется для итерации по хеш-таблице
;; maphash необходимо передать функцию 2-х аргументов и саму таблицу

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)

;; => GIANT

(maphash #'(lambda (k v)
             (format t "~A = ~A~%" k v))
           ht)

;; CL-USER> (maphash #'(lambda (k v)
;;                       (format t "~A = ~A~%" k v))
;;                   ht)
;; COLOR = RED
;; SHAPE = SPHERICAL
;; SIZE = GIANT
;; NIL

;;---------------------------------------------------
;; :size - ключ функции make-hash-table
;; Он задаёт исходную ёмкость таблицы (количество пар ключ-значение)

(make-hash-table :size 5)

;; => #<HASH-TABLE :TEST EQL :COUNT 0 {C490BB1}>

;;---------------------------------------------------
;; :test - ключ функции make-hash-table, позволяет использовать различные
;; предикаты проверки эквивалентности

(setf writers (make-hash-table :test #'equal))

;; => #<HASH-TABLE :TEST EQUAL :COUNT 0 {C67D6A1}>

(setf (gethash '(ralph waldo emerson) writers) t)

;; => T
