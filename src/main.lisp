(uiop:define-package #:lang
  (:nicknames #:lang/main)
  (:use #:cl)
  (:use-reexport #:lang/ast)
  (:use-reexport #:lang/evaluator))
(in-package #:lang)
