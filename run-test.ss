#lang scheme
(require rackunit "run.ss")


(check-equal? (run '((function (x) (* x  x)) 3)) 9 "test1")
(check-equal? (run '((function (x y) (+ x  y)) 3 4)) 7 "test2")
(check-equal? (run '((function (x y) (* x  y)) 3 4)) 12 "test3")
(check-equal? (run '((function (x y z) (+(+ x y) z)) 3 4 15)) 22 "test4")
(check-equal? (run '((function (x y z w) (+ x y)) 3 4 5 7 )) 7 "test5")
(check-equal? (run '((function (x y z w) (< x 2)) 3 4 5 7 )) #f "test6")
(check-equal? (run '((function (x y z w) (< 3 2)) 3 4 5 7 )) #f "test7")
(check-equal? (run '((function (x y z w) (+ (* (/ x y) z)w)) 12 4 5 7 )) 22 "test8")




