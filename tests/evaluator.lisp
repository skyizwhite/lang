(defpackage #:lang-tests/evaluator
  (:use #:cl
        #:rove
        #:lang/ast
        #:lang/evaluator))
(in-package #:lang-tests/evaluator)

(deftest check-evaluating-bin-math-expr
  (testing "1 + 1 == 2"
    (ok (= (evaluate (make-add (make-int 1)
                               (make-int 1))
                     (make-hash-table))
           2)))

  (testing "1 - 2 == -1"
    (ok (= (evaluate (make-sub (make-int 1)
                               (make-int 2))
                     (make-hash-table))
           -1)))

  (testing "2 * 3 == 6"
    (ok (= (evaluate (make-mul (make-int 2)
                               (make-int 3))
                     (make-hash-table))
           6)))

  (testing "6 / 2 == 3"
    (ok (= (evaluate (make-div (make-int 6)
                               (make-int 2))
                     (make-hash-table))
           3)))

  (testing "1 / 0 == Error!"
    (signals (evaluate (make-div (make-int 1)
                                 (make-int 0))
                       (make-hash-table))))

  (testing "(1 + (2 * 3) - 1) / 2 == 3"
    (ok (= (evaluate (make-div (make-sub (make-add (make-int 1)
                                                   (make-mul (make-int 2)
                                                             (make-int 3)))
                                         (make-int 1))
                               (make-int 2))
                     (make-hash-table))
           3))))

(deftest check-evaluating-bin-comp-expr
  (testing "1 < 2 == 1"
    (ok (= (evaluate (make-lt (make-int 1)
                              (make-int 2))
                     (make-hash-table))
           1)))

  (testing "2 > 1 == 1"
    (ok (= (evaluate (make-gt (make-int 2)
                              (make-int 1))
                     (make-hash-table))
           1)))

  (testing "1 <= 1 == 1"
    (ok (= (evaluate (make-gte (make-int 1)
                               (make-int 1))
                     (make-hash-table))
           1)))

  (testing "1 >= 1 == 1"
    (ok (= (evaluate (make-lte (make-int 1)
                               (make-int 1))
                     (make-hash-table))
           1)))

  (testing "1 == 1 == 1"
    (ok (= (evaluate (make-eq (make-int 1)
                              (make-int 1))
                     (make-hash-table))
           1)))

  (testing "1 != 0 == 1"
    (ok (= (evaluate (make-ne (make-int 1)
                              (make-int 0))
                     (make-hash-table))
           1))))
