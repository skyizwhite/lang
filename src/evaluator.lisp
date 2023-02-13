(defpackage #:lang/evaluator
  (:use #:cl
        #:alexandria
        #:lang/ast)
  (:export #:evaluate))
(in-package #:lang/evaluator)

(defgeneric evaluate (e env)
  (:documentation "Evaluate AST"))

(defmethod evaluate ((e l-program) env)
  (dolist (func (l-program-functions e))
    (setf (gethash (l-func-name func) env) func))
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
  (let ((condition (l-while-condition e)))
    (while (evaluate condition env)
      (dolist (body (l-while-bodies e))
        (evaluate body env))
      (setf condition (evaluate (l-while-condition e) env)))
    nil))

(defmethod evaluate ((e l-assignment) env)
  (setf (gethash (l-assignment-name e) env)
        (evaluate (l-assignment-expression e) env)))

(defmethod evaluate ((e l-ident) env)
  (gethash (l-ident-name e) env))

(defmethod evaluate ((e l-call) env)
  (let ((func (evaluate (l-call-name e) env))
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

(defmacro evaluate-bin-math-expr (op e env)
  `(funcall ,op
            (evaluate (l-bin-expr-lhs ,e) ,env)
            (evaluate (l-bin-expr-rhs ,e) ,env)))

(defmacro evaluate-bin-comp-expr (op e env)
  `(if (evaluate-bin-math-expr ,op ,e ,env)
       1
       0))

(defmethod evaluate ((e l-bin-expr) env)
  (case (l-bin-expr-op e)
    (+ (evaluate-bin-math-expr #'+ e env))
    (- (evaluate-bin-math-expr #'- e env))
    (* (evaluate-bin-math-expr #'* e env))
    (/ (evaluate-bin-math-expr #'/ e env))
    (< (evaluate-bin-comp-expr #'< e env))
    (> (evaluate-bin-comp-expr #'> e env))
    (<= (evaluate-bin-comp-expr #'<= e env))
    (>= (evaluate-bin-comp-expr #'>= e env))
    (== (evaluate-bin-comp-expr #'= e env))
    (!= (evaluate-bin-comp-expr (complement #'=) e env))
    (otherwise (format t "what?"))))
