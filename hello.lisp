;; Напишите на любом известном вам языке программу,
;; которая для каждого числа от 1 до 100:
;; 1. если число делится на 3, печатает слово “Hello”,
;; 2. если число делится на 5, выводит слово “world”,
;; 3. если число делится и на 3, и на 5, печатает фразу “Hello world”,
;; иначе печатает само число.
;; Всё.



(defun test (x)
  (if (= 0 (cadr (multiple-value-list (floor x 15))))
      (print "helo word")
      (if (= 0 (cadr (multiple-value-list (floor x 3))))
          (print "helo")
          (if (= 0 (cadr (multiple-value-list (floor x 5))))
              (print "word")
              (print x)))))

(loop for x from 1 to 100 do (test x))

