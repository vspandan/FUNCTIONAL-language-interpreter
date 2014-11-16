#lang scheme

;;; =====================================
;;; Evaluator for the language FUNCTIONAL
;;; =====================================
  
(require eopl/eopl)
(require "ast.ss")
(require "env-proc.ss")
(require "semantic-domains.ss")


(provide
   eval-ast)

;;; eval-ast : [ast? env?]-> expressible-value?
;;; eval-ast :  throws error

(define eval-ast
  (lambda (a env)
    (cases ast a
      [number (datum) datum]
      [boolean (datum) datum]
      [id-ref (sym) (lookup-env env sym)]
      [ifte (test then else-ast)
        (let([b (eval-ast test env)])
          (if (boolean? b)
            (eval-ast (if b then else-ast) env)
            (error 'eval-ast "ifte test is not a boolean ~a" a)))]

      ;;; ADD CODE FOR EVAL-AST OF FUNCTION HERE
      [function (formals body)
          (closure formals body env)]
      
      [app (rator rands)
        (let ([p (eval-ast rator env)]
              [args (map
                      (lambda (rand)
                        (eval-ast rand env))
                      rands)])
          (if (proc? p)
            (apply-proc p args)
            (error 'eval-ast "application rator is not a proc ~a" a)))]
      
      [assume (binds body)
        (let* ([ids  (map bind-id binds)]
               [asts (map bind-ast binds)]
               [vals (map (lambda (a)
                            (eval-ast a env))
                          asts)]
               [new-env
                (extended-env ids vals env)])
          (eval-ast body new-env))]
      [else (error 'eval-ast "unable to handle some ast cases")])))




;;; apply-proc :
;;;  [proc? (list-of expressible-value?)]
;;;    -> expressible-value?

(define apply-proc
  (lambda (p args)
    (cases proc p
      [prim-proc (prim sig)
        (apply-prim-proc prim sig args)]
      
      [closure (formals body env)
        (apply-closure formals body env args)])))


;;; apply-prim-proc : takes a procedure (the built in Scheme
;;; procedure corresponding to the primitive, the signature
;;; of the primitive, which is a list of type predicates,
;;; and a list of arguments, which are expressible values.
;;; apply-prim-proc :
;;;  [procedure? (listof procedure?)
;;;     (listof expressible-value?)] -> expressible-value?
;;;
;;; apply-prim-proc : throws error when number or type of
;;;     args do not match the signature of prim-proc

;;; CLEAN UP the definition APPLY-PRIM-PROC 
;;; given as a dummy below
(define apply-prim-proc
    (lambda (prim sig args)
      (let* ([args-sig (rest sig)])
        (cond
         [(and
            (= (length args-sig) (length args))
            (andmap match-arg-type args-sig args))
          (apply prim  args)]
         [#t (error 'apply-prim-proc
               "invalid arguments ~a"
               prim)]))))

;;; match-arg-type : [procedure? any/c] -> boolean?
(define match-arg-type
  (lambda (arg-type val)
    (arg-type val)))

;;; fix the signature and cleanup the definition of
;;; apply-closure given as a dummy below
(define apply-closure
    (lambda (formals body env args)
      (let ([new-env (extended-env formals args env)])
        (eval-ast body new-env))))

;;; Unit testing
;;; ============

(require rackunit)

(define e1
  (extended-env '(x y z) '(1 2 3) (empty-env)))

(let ([n5 (number 5)]
      [n7 (number 7)]
      [bt (boolean #t)]
      [id1 (id-ref 'x)]
      [id2 (id-ref 'y)]
      [id3 (id-ref 'z)])
  (check-equal? (eval-ast n5 e1) 5 "eval-ast: n5 test")
  (check-equal? (eval-ast bt e1) #t "eval-ast: bt test")
  (check-equal? (eval-ast id1 e1) 1 "eva-ast: x test")
  (check-equal? (eval-ast id2 e1) 2 "eval-ast: y test"))

;;; These tests should pass once you've fixed eval-ast.ss and
;;; uncommented the code below
 (check-equal?
   (eval-ast
     (app (function '(x) (id-ref 'x)) (list (number 3))) e1)
   3 "eval-ast: app-function test")

 (check-exn exn:fail?
            (lambda () (eval-ast (app (number 5) (list (number 3))) (empty-env)))
            "app-fail-test") ;; fails because 5 is not a proc.