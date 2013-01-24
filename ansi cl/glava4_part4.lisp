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
;; What kind of polemic was it?

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
