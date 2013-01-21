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




