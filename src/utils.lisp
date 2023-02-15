(defpackage #:lang/utils
  (:use #:cl)
  (:import-from #:cl-annot-revisit)
  (:export #:symbol-append))
(in-package #:lang/utils)

(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))
