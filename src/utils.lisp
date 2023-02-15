(defpackage #:lang/utils
  (:use #:cl)
  (:export #:symbol-append))
(in-package #:lang/utils)

(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))
