;; Макросы, для того чтобы разобраться с комментариями для motoinf.ru (сделано Мишей)

(defmacro def-bla (name param &body body)
  `(progn
     (restas:define-route ,name ,param
       ,@body)))

(print (macroexpand-1 '(def-bla my-name ("my-param")
                        (list 1 2 3))))

;; => (PROGN
;;      (RESTAS:DEFINE-ROUTE MY-NAME ("my-param")
;;        (LIST 1 2 3)))

(defmacro obertka (name param &body body)
  `(progn
     (def-bla name param ,@body)
     (my-comment "blablabla")))


(print (macroexpand-1 '(obertka my-name ("my-param")
                        (list 1 2 3))))

;; => (PROGN
;;      (DEF-BLA NAME PARAM
;;        (LIST 1 2 3))
;;      (MY-COMMENT "blablabla"))


;; =>  (PROGN
;;       (PROGN
;;         (RESTAS:DEFINE-ROUTE MY-NAME ("my-param")
;;           (LIST 1 2 3)))
;;       (MY-COMMENT "blablabla"))
