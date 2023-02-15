(defpackage #:lang/evaluator
  (:use #:cl
        #:lang/ast)
  (:import-from #:alexandria
                #:copy-hash-table)
  (:export #:evaluate))
(in-package #:lang/evaluator)

(defgeneric evaluate (e env)
  (:documentation "Evaluate AST"))

(defmethod evaluate ((e l-program) env)
  (dolist (f (l-program-functions e))
    (setf (gethash (l-func-name f) env) f))
  (let ((result))
    (dolist (body (l-program-bodies e) result)
      (setf result (evaluate body env)))))

(defmethod evaluate ((e l-seq) env)
  (let ((result))
    (dolist (body (l-seq-bodies e) result)
      (setf result (evaluate body env)))))
        
(defmethod evaluate ((e l-if) env)
  (if (evaluate (l-if-condition e) env)
      (evaluate (l-if-then e) env)
      (evaluate (l-if-else e) env)))

(defmacro while (condition &body body)
  `(loop :while ,condition
         :do (progn ,@body)))

(defmethod evaluate ((e l-while) env)
  (let ((condition (evaluate (l-while-condition e) env)))
    (while condition
      (dolist (body (l-while-bodies e))
        (evaluate body env))
      (setf condition (evaluate (l-while-condition e) env)))))

(defmethod evaluate ((e l-assign) env)
  (setf (gethash (l-assign-name e) env)
        (evaluate (l-assign-expression e) env)))

(defmethod evaluate ((e l-id) env)
  (gethash (l-id-name e) env))

(defmethod evaluate ((e l-call) env)
  (let ((func (gethash (l-call-name e) env))
        (args (mapcar #'(lambda (arg)
                          (evaluate arg env))
                      (l-call-args e)))
        (new-env (copy-hash-table env)))
    (loop :for i :from 0
          :for arg :in args
          :do (setf (gethash (nth i (l-func-params func))
                             new-env)
                    arg))
    (evaluate (l-func-body func) new-env)))

(defmethod evaluate ((e l-int) env)
  (l-int-value e))

(defmacro evaluate-bin-expr (op e env)
  `(funcall ,op
            (evaluate (l-bin-expr-lhs ,e) ,env)
            (evaluate (l-bin-expr-rhs ,e) ,env)))

(defmethod evaluate ((e l-bin-expr) env)
  (let ((op (l-bin-expr-op e)))
    (cond ((string= op "+") (evaluate-bin-expr #'+ e env))
          ((string= op "-") (evaluate-bin-expr #'- e env))
          ((string= op "*") (evaluate-bin-expr #'* e env))
          ((string= op "/") (evaluate-bin-expr #'/ e env))
          ((string= op "<") (evaluate-bin-expr #'< e env))
          ((string= op ">") (evaluate-bin-expr #'> e env))
          ((string= op "<=") (evaluate-bin-expr #'<= e env))
          ((string= op ">=") (evaluate-bin-expr #'>= e env))
          ((string= op "==") (evaluate-bin-expr #'= e env))
          ((string= op "!=") (evaluate-bin-expr (complement #'=) e env))
          (t (format t "what?")))))
