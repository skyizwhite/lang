(defpackage #:lang/ast
  (:use #:cl)
  (:import-from #:cl-annot-revisit))
(in-package #:lang/ast)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

@cl-annot-revisit:export-structure
(defstruct (l-program
             (:constructor t-program (functions &rest bodies)))
  functions
  bodies)

@cl-annot-revisit:export-structure
(defstruct (l-func
             (:constructor t-func (name params body)))
  name
  params
  body)

@cl-annot-revisit:export-structure
(defstruct (l-if
             (:constructor t-if (condition then else)))
  condition
  then
  else)

@cl-annot-revisit:export-structure
(defstruct (l-seq
             (:constructor t-seq (&rest bodies)))
  bodies)

@cl-annot-revisit:export-structure
(defstruct (l-while
             (:constructor t-while (condition &rest bodies)))
  condition
  bodies)

@cl-annot-revisit:export-structure
(defstruct (l-call
             (:constructor t-call (name &rest args)))
  name
  args)

@cl-annot-revisit:export-structure
(defstruct (l-assignment
             (:constructor t-assign (name expression)))
  name
  expression)

@cl-annot-revisit:export-structure
(defstruct (l-int
             (:constructor t-int (value)))
  value)

@cl-annot-revisit:export-structure
(defstruct (l-ident
             (:constructor t-id (name)))
  name)

@cl-annot-revisit:eval-always
(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))

@cl-annot-revisit:eval-always
(defmacro define-bin-expr (name op)
  `(cl-annot-revisit:export
     (defun ,(symbol-append 't- name) (a b)
       (t-bin-expr ,op a b))))

@cl-annot-revisit:export-structure
(defstruct (l-bin-expr
             (:constructor t-bin-expr (op lhs rhs)))
  op
  lhs
  rhs)

(define-bin-expr add "+")
(define-bin-expr sub "-")
(define-bin-expr mul "*")
(define-bin-expr div "/")
(define-bin-expr lt "<")
(define-bin-expr gt ">")
(define-bin-expr lte "<=")
(define-bin-expr gte ">=")
(define-bin-expr eq "==")
(define-bin-expr ne "!=")
