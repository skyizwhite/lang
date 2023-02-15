(defsystem "lang-tests"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "lang-tests/macros"
               "lang-tests/evaluator")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
