(defsystem "lang"
  :description "Very simple interpreter"
  :author "paku (skyizwhite)"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("lang/main")
  :in-order-to ((test-op (test-op "lang-tests"))))
