(defsystem "lang-tests"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "lang-tests/main")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
