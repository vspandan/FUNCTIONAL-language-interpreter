#lang scheme

(require "ast.ss")
(require "env-proc.ss")
(require "semantic-domains.ss")
(require "eval-ast.ss")
(require "parser.ss")

(provide
 run
 *init-env*)


;;; Primitive Procedures
;;; ====================

(define nonzero? (and/c number? (not/c zero?)))


;;; uncomment and test once you've got prim-proc defined correctly
; (define +p
;   (prim-proc +
;     (list
;       number? ; result type
;       number? ; argument-1 type
;       number? ; argument-2 type
;       )))

 (define +p
    (prim-proc +
      (list number? number? number?)))

(define -p
    (prim-proc -
      (list number? number? number?)))

(define *p
    (prim-proc *
      (list number? number? number?)))

(define /p
    (prim-proc /
      (list number? number? nonzero?)))


(define <p
    (prim-proc  <
      (list boolean? number? number?)))

;;; scheme <=
(define <=p
    (prim-proc   <=
      (list boolean? number? number?)))

;;; checks if two expressible values are equal using the
;;; scheme eq? predicate
(define eq?p
    (prim-proc eq?
      (list boolean? expressible-value? expressible-value?)))

;;; checks if the argument is a number equal to zero.
(define 0?p
    (prim-proc zero?
      (list boolean? number?)))


;;; Fix this definition to include all the above defined
;;; primitives.

(define *init-env*
    (extended-env
     '(+ - * / < <= eq? 0?)
     (list +p -p *p /p <p <=p eq?p 0?p)
     (empty-env)))


;;; run: ast? -> expressible-value?

(define run
  (lambda (ast)
    (eval-ast (parse ast) *init-env*)))


;;; Unit testing
;;; ============

(require rackunit)


;;; These tests should run once you fix the definition of
;;; *init-env*
;;; Make sure you uncomment them and test!

; (check-equal?
;   (run
;       (assume (list (make-bind 'a (number 5))
;                 (make-bind 'b (number 6)))
;         (app (id-ref '+)
;           (list (id-ref 'a) (id-ref 'b)))))
;   11 "run: assume-test")


; (check-equal?
;  (run
;   (function     ; (function (x y z) (+ x (* y z)))
;    '(x y z)
;    (app (id-ref '+)
;         (list (id-ref 'x)
;               (app (id-ref '*)
;                    (list (id-ref 'y) (id-ref 'z)))))))
;   (closure '(x y z)
;     (app (id-ref '+)
;       (list (id-ref 'x)
;         (app (id-ref '*)
;           (list (id-ref 'y) (id-ref 'z)))))
;     *init-env*)
;   "run: function-test")


; (check-equal?
;  (run

;   (function     ; (function (x y z) (+ x (* y z)))
;    '(x y z)
;    (app (id-ref '+)
;         (list (id-ref 'x)
;               (app (id-ref '*)
;                    (list (id-ref 'y) (id-ref 'z)))))))
;   (closure '(x y z)
;     (app (id-ref '+)
;       (list (id-ref 'x)
;         (app (id-ref '*)
;           (list (id-ref 'y) (id-ref 'z)))))
;     *init-env*)
;   "run: function-test")


