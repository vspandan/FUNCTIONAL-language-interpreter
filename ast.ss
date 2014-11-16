#lang scheme

;;; ===========================================
;;; Abstract Syntax for the language FUNCTIONAL
;;; ===========================================



;;; <ast> ::= <num-ast> |
;;;           <bool-ast> |
;;;           <id-ref-ast> |
;;;           <ifte-ast> |
;;;           <assume-ast> |
;;;           <function-ast> |
;;;           <app-ast>


;;; <num-ast>  ::= (number <number>)
;;; <bool-ast> ::= (boolean <boolean>)
;;; <assume-ast>   ::=(assume ((<id> <ast>) ...) <ast>)
;;; <id-ref-ast>   ::= (id-ref <id>)
;;; <id>           ::= <symbol>
;;; <ifte-ast>     ::= (ifte <ast> <ast> <ast>)
;;; <function-ast> ::= (function (<id> ... ) <ast>)
;;; <app-ast>      ::= (app  <ast>  <ast> ...)

(require eopl/eopl)

(provide
  ast
  ast?
  number
  boolean
  id-ref
  ifte
  function
  app
  assume
  make-bind
  bind-id
  bind-ast
  check-ast?)

(define-datatype ast ast?
  [number (datum number?)]
  [boolean (datum boolean?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [function
   (formals (list-of id?))
   (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  [id-ref (sym id?)]
  [assume (binds  (list-of bind?)) (body ast?)])

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

(define id? symbol?)

;;; unit Testing
;;; ============

;;; Racket's unit testing framework
(require rackunit)


(define-simple-check (check-ast? thing)
   (ast? thing))

(check-ast? (number 5) "number-5 test")
(check-ast? (boolean #t) "boolean-#t test")
(check-ast? (id-ref 'x) "id-ref-x test")


(check-ast?
  (ifte (id-ref 'x)
    (app (id-ref 'x) (list (number 4)))
    (boolean #f))
  "ifte-test1")


(check-exn
 exn:fail?
 (lambda ()
   (ifte (id-ref 'x) ; ifte is incorrectly
     ; invoked with only two
     ; arguments.
     (boolean #f)))
 "ifte-test2")

(check-ast? (function
              '(x y z)
              (app (id-ref '+)
                (list (id-ref 'x)
                  (app (id-ref '*)
                    (list (id-ref 'y) (id-ref 'z)))))) "function-test")

(check-ast?
  (app (id-ref '+)
    (list (number 5) (number 6))) "app test")


(check-exn    ;; app constructor is incorrectly invoked
 exn:fail?
  (lambda () 
    (app (id-ref '+))) "app test")


(check-ast?
  (assume (list (make-bind 'x (number 5))
            (make-bind 'y (number 6)))
    (app (id-ref '+)
      (list (id-ref 'x) (id-ref 'y)))) "assume-test")












