(defpackage #:lang-tests/macros
  (:use #:cl
        #:rove)
  (:import-from #:lang/ast
                #:defnode))
(in-package #:lang-tests/macros)

(deftest check-defnode
  (testing "Define node with props and rest."
    (ok
     (expands '(defnode hoge
                :props (ch1 ch2)
                :rest bodies)
              '(cl-annot-revisit:export-structure
                (defstruct (l-hoge
                             (:constructor t-hoge (ch1 ch2 &rest bodies)))
                  ch1
                  ch2
                  bodies)))))

  (testing "Define node with props."
    (ok
     (expands '(defnode hoge
                :props (ch1 ch2))
              '(cl-annot-revisit:export-structure
                (defstruct (l-hoge
                             (:constructor t-hoge (ch1 ch2)))
                  ch1
                  ch2)))))

  (testing "Define node with rest."
    (ok
     (expands '(defnode hoge
                :rest bodies)
              '(cl-annot-revisit:export-structure
                (defstruct (l-hoge
                             (:constructor t-hoge (&rest bodies)))
                  bodies))))))
