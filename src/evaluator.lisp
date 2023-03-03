(defpackage #:lang/evaluator
  (:use #:cl
        #:lang/ast)
  (:import-from #:alexandria
                #:copy-hash-table)
  (:export #:evaluate))
(in-package #:lang/evaluator)

(defgeneric evaluate (ast env)
  (:documentation "Evaluate AST"))

(defmethod evaluate ((ast l-program) env)
  (dolist (f (l-program-functions ast))
    (setf (gethash (l-func-name f) env) f))
  (let ((result))
    (dolist (body (l-program-bodies ast) result)
      (setf result (evaluate body env)))))

(defmethod evaluate ((ast l-seq) env)
  (let ((result))
    (dolist (body (l-seq-bodies ast) result)
      (setf result (evaluate body env)))))
        
(defmethod evaluate ((ast l-if) env)
  (if (evaluate (l-if-condition ast) env)
      (evaluate (l-if-then ast) env)
      (evaluate (l-if-else ast) env)))

(defmacro while (condition &body body)
  `(loop :while ,condition
         :do (progn ,@body)))

(defmethod evaluate ((ast l-while) env)
  (let ((condition (evaluate (l-while-condition ast) env)))
    (while condition
      (dolist (body (l-while-bodies ast))
        (evaluate body env))
      (setf condition (evaluate (l-while-condition ast) env)))))

(defmethod evaluate ((ast l-assign) env)
  (setf (gethash (l-assign-name ast) env)
        (evaluate (l-assign-expression ast) env)))

(defmethod evaluate ((ast l-id) env)
  (gethash (l-id-name ast) env))

(defmethod evaluate ((ast l-call) env)
  (let ((func (gethash (l-call-name ast) env))
        (args (mapcar #'(lambda (arg)
                          (evaluate arg env))
                      (l-call-args ast)))
        (new-env (copy-hash-table env)))
    (loop :for i :from 0
          :for arg :in args
          :do (setf (gethash (nth i (l-func-params func))
                             new-env)
                    arg))
    (evaluate (l-func-body func) new-env)))

(defmethod evaluate ((ast l-int) env)
  (l-int-value ast))

(defun evaluate-bin-expr (op ast env)
  (funcall op
           (evaluate (l-bin-expr-lhs ast) env)
           (evaluate (l-bin-expr-rhs ast) env)))

(defmethod evaluate ((ast l-bin-expr) env)
  (let ((op (l-bin-expr-op ast)))
    (cond ((string= op "+") (evaluate-bin-expr #'+ ast env))
          ((string= op "-") (evaluate-bin-expr #'- ast env))
          ((string= op "*") (evaluate-bin-expr #'* ast env))
          ((string= op "/") (evaluate-bin-expr #'/ ast env))
          ((string= op "<") (evaluate-bin-expr #'< ast env))
          ((string= op ">") (evaluate-bin-expr #'> ast env))
          ((string= op "<=") (evaluate-bin-expr #'<= ast env))
          ((string= op ">=") (evaluate-bin-expr #'>= ast env))
          ((string= op "==") (evaluate-bin-expr #'= ast env))
          ((string= op "!=") (evaluate-bin-expr (complement #'=) ast env))
          (t (format t "what?")))))
