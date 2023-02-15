(defpackage #:lang/ast
  (:use #:cl)
  (:import-from #:cl-annot-revisit)
  (:import-from #:lang/utils
                #:symbol-append)
  (:export #:defnode
           #:define-bin-expr))
(in-package #:lang/ast)

(defmacro defnode (name &key children rest)
  `(cl-annot-revisit:export-structure
     (defstruct (,(symbol-append 'l- name)
                  (:constructor
                   ,(symbol-append 't- name)
                   (,@(append  children (and rest `(&rest ,rest))))))
       ,@(append children (and rest (list rest))))))

(defnode program
  :children (functions)
  :rest bodies)

(defnode func
  :children (name params body))

(defnode if
  :children (condition then else))

(defnode seq
  :rest bodies)

(defnode while
  :children (condition)
  :rest bodies)

(defnode call
  :children (name)
  :rest args)

(defnode assign
  :children (name expression))

(defnode int
  :children (value))

(defnode id
  :children (name))

(defnode bin-expr
  :children (op lhs rhs))

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
