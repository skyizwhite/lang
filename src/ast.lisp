(defpackage #:lang/ast
  (:use #:cl)
  (:import-from #:cl-annot-revisit))
(in-package #:lang/ast)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

@cl-annot-revisit:export-structure
(defstruct (l-program
             (:constructor make-program (functions &rest bodies)))
  functions
  bodies)

@cl-annot-revisit:export-structure
(defstruct (l-func
             (:constructor make-func (name params body)))
  name
  params
  body)

@cl-annot-revisit:export-structure
(defstruct (l-if
             (:constructor make-if (condition then else)))
  condition
  then
  else)

@cl-annot-revisit:export-structure
(defstruct (l-seq
             (:constructor make-seq (&rest bodies)))
  bodies)

@cl-annot-revisit:export-structure
(defstruct (l-while
             (:constructor make-while (condition &rest bodies)))
  condition
  bodies)

@cl-annot-revisit:export-structure
(defstruct (l-call
             (:constructor make-call (name &rest args)))
  name
  args)

@cl-annot-revisit:export-structure
(defstruct (l-assignment
             (:constructor make-assignment (name expression)))
  name
  expression)

@cl-annot-revisit:export-structure
(defstruct (l-int
             (:constructor make-int (value)))
  value)

@cl-annot-revisit:export-structure
(defstruct (l-ident
             (:constructor make-ident (name)))
  name)

@cl-annot-revisit:eval-always
(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))

@cl-annot-revisit:eval-always
(defmacro define-bin-expr (name op)
  `(cl-annot-revisit:export
     (defun ,(symbol-append 'make- name) (a b)
       (make-bin-expr ,op a b))))

@cl-annot-revisit:export-structure
(defstruct (l-bin-expr
             (:constructor make-bin-expr (op lhs rhs)))
  op
  lhs
  rhs)

(define-bin-expr add '+)
(define-bin-expr sub '-)
(define-bin-expr mul '*)
(define-bin-expr div '/)
(define-bin-expr lt '<)
(define-bin-expr gt '>)
(define-bin-expr lte '<=)
(define-bin-expr gte '>=)
(define-bin-expr eq '==)
(define-bin-expr ne '!=)
