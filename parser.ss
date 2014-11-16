#lang scheme

;;; ==================================
;;; Parser for the language FUNCTIONAL
;;; ==================================

;;; Concrete Syntax
;;; ---------------

;;;  <exp>  ::= <number> |
;;;             <boolean> |
;;;             <id>  |
;;;             (ifte <exp> <exp> <exp>) |
;;;             (assume ((<id> <exp>) ...) <exp>) |
;;;             (function (<id> ...) <exp>) |
;;;             (<exp> <exp> ...)

(require racket/match)

(require "ast.ss")

(provide
  parse)

(define *keywords*
  '(ifte function assume))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))
         

(define parse
  (lambda (d)
    (match d
     [(? number? n) (number n)]
     [(? boolean? b) (boolean b)]
     [(? id? x) (id-ref x)]
     [(list 'ifte a b c)  (ifte (parse a) (parse b) (parse c))]

     [(list 'assume
        (list (list (? id? x) e) ...) body)
      (let* ([a (map parse e)]
             [b (map make-bind x a)])
        (assume b (parse body)))]
     
     ;;; COMPLETE THE CASE FOR FUNCTION
     ;;; COMPLETE THE CASE FOR APPLICATION
     [(list
         'function
         (list (? id? x) ...)
         body)
        (function x (parse body))]
        
     
      
     [(list rator rands ...)
        (let* ([rator (parse rator)]
               [rands (map parse rands)])
          (app rator rands))]
     
     [_ (error 'parse "don't know how to parse ~a" d)])))



;;; Unit Testing
;;; ============
(require rackunit)

(check-equal? (parse 4) (number 4) "parse-number")
(check-equal? (parse #t) (boolean #t) "parse-boolean")
(check-equal? (parse 'x) (id-ref 'x) "parse-id")

(check-equal?
 (parse '(ifte 3 4 8))
 (ifte (number 3) (number 4) (number 8))
 "parse-ifte")

(check-equal?
  (parse '(assume ([x 3]) 6))
  (assume (list (make-bind 'x (number 3))) (number 6))
  "parse-assume-1")

;;; these test cases should pass once you have correctly
;;; implemented the parser and uncommented the code below:

 (check-equal?
  (parse '(function (x y) 4))
  (function '(x y) (number 4))
  "parse-function")

 (check-equal?
   (parse '(x y))
   (app (id-ref 'x)
        (list (id-ref 'y)))
   "parse-app")

 (check-equal?
  (parse '(assume ((x 5) (y 6))
            (+ x y)))
  (assume (list (make-bind 'x (number 5))
            (make-bind 'y (number 6)))
    (app (id-ref '+)
      (list (id-ref 'x) (id-ref 'y)))) "parse-assume-2")


(check-exn exn? (lambda () (parse "hello")) "parse-string-error")
(check-exn exn? (lambda () (parse '#(1 2))) "parse-vector-error")
(check-exn exn? (lambda () (parse '(1 . 2))) "parse-cons-error")
(check-exn exn? (lambda () (parse '())) "parse-empty-error")
(check-exn exn? (lambda () (parse '(ifte #t x))) "parse-ifte-error")
(check-exn exn? (lambda () (parse '(ifte #t x))) "parse-ifte-error")
(check-exn exn? (lambda () (parse '(assume (+  x y)))) "parse-assume-error")


