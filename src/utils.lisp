(defpackage #:lang/utils
  (:use #:cl)
  (:import-from #:cl-annot-revisit)
  (:export #:symbol-append))
(in-package #:lang/utils)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

@cl-annot-revisit:eval-always
(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))
