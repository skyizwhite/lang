(defpackage #:lang/ast
  (:use #:cl)
  (:import-from #:cl-annot-revisit)
  (:import-from #:lang/utils
                #:symbol-append)
  (:export #:defnode
           #:define-bin-expr))
(in-package #:lang/ast)

(defmacro defnode (name &key props rest)
  `(cl-annot-revisit:export-structure
     (defstruct (,(symbol-append 'l- name)
                  (:constructor
                   ,(symbol-append 't- name)
                   (,@(append props (and rest `(&rest ,rest))))))
       ,@(append props (and rest (list rest))))))

(defnode program
  :props (functions)
  :rest bodies)

(defnode func
  :props (name params body))

(defnode if
  :props (condition then else))

(defnode seq
  :rest bodies)

(defnode while
  :props (condition)
  :rest bodies)

(defnode call
  :props (name)
  :rest args)

(defnode assign
  :props (name expression))

(defnode int
  :props (value))

(defnode id
  :props (name))

(defnode bin-expr
  :props (op lhs rhs))

(defmacro define-bin-expr (name op)
  `(cl-annot-revisit:export
     (defun ,(symbol-append 't- name) (a b)
       (t-bin-expr ,op a b))))

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
