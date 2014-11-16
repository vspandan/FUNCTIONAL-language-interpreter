#lang scheme

;;; ============================================
;;; Semantic Domains for the language FUNCTIONAL
;;; ============================================


;;; Expressible Values (types of values returned by
;;; evaluating an ast)

;;; ========================================

;;; expressible-value ::=
;;;    number | boolean | proc

;;; Denotable Values (types of values denoted by
;;; identifiers)
;;; ============================================

;;; denotable-value ::=
;;;  number | boolean | proc

(provide
  proc
  prim-proc
  closure
  
  proc?
  prim-proc?
  closure?
  
  expressible-value?
  denotable-value?
  )

;;; expressible-value? is the set of things that are the
;;; results of evaluation of expressions (asts).


(require eopl/eopl)
(require "ast.ss")
(require "env-proc.ss")

(define-datatype proc proc?
  [prim-proc
    ;; prim refers to a scheme procedure
    (prim procedure?)
    ;; sig is the signature
    (sig (list-of procedure?))] 
  [closure
    (formals (list-of symbol?))
    (body ast?)
    (env env?)])

;;; prim? : proc? -> boolean?

(define prim-proc?
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #t]
      [else #f])))

(define closure? 
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #f]
      [else #t])))

;;; complete the definition of expressible-value?

(define expressible-value?
  (lambda (x)
    (or (number? x)
      (boolean? x))))

;;; complete the definition of denotable-value?

(define denotable-value?
  (lambda (x)
    (or (number? x)
      (boolean? x))))




