(defpackage #:lang-tests
  (:nicknames #:lang-tests/main)
  (:use #:cl
        #:rove
        #:lang))
(in-package #:lang-tests)

(deftest check-evaluating-bin-expr
  (testing "1 + 1 == 2"
    (ok (= (evaluate (t-add (t-int 1)
                            (t-int 1))
                     (make-hash-table))
           2)))
  
  (testing "1 - 2 == -1"
    (ok (= (evaluate (t-sub (t-int 1)
                            (t-int 2))
                     (make-hash-table))
           -1)))
  
  (testing "2 * 3 == 6"
    (ok (= (evaluate (t-mul (t-int 2)
                            (t-int 3))
                     (make-hash-table))
           6)))
  
  (testing "6 / 2 == 3"
    (ok (= (evaluate (t-div (t-int 6)
                            (t-int 2))
                     (make-hash-table))
           3)))
  
  (testing "1 / 0 == Error!"
    (signals (evaluate (t-div (t-int 1)
                              (t-int 0))
                       (make-hash-table))))
  
  (testing "(1 + (2 * 3) - 1) / 2 == 3"
    (ok (= (evaluate (t-div (t-sub (t-add (t-int 1)
                                          (t-mul (t-int 2)
                                                 (t-int 3)))
                                   (t-int 1))
                            (t-int 2))
                     (make-hash-table))
           3)))
  
  (testing "1 < 2 == 1"
    (ok (evaluate (t-lt (t-int 1)
                        (t-int 2))
                  (make-hash-table))))
  
  (testing "2 > 1 == 1"
    (ok (evaluate (t-gt (t-int 2)
                        (t-int 1))
                  (make-hash-table))))
  
  (testing "1 <= 1 == 1"
    (ok (evaluate (t-gte (t-int 1)
                         (t-int 1))
                  (make-hash-table))))
  
  (testing "1 >= 1 == 1"
    (ok (evaluate (t-lte (t-int 1)
                         (t-int 1))
                  (make-hash-table))))
  
  (testing "1 == 1 == 1"
    (ok (evaluate (t-eq (t-int 1)
                        (t-int 1))
                  (make-hash-table))))
  
  (testing "1 !=e 0 == 1"
    (ok (evaluate (t-ne (t-int 1)
                        (t-int 0))
                  (make-hash-table)))))

(deftest check-evaluating-assignment
  (testing "{a = 100; a} == 100"
    (ok (= (evaluate (t-seq (t-assign 'a
                                      (t-int 100))
                            (t-id 'a))
                     (make-hash-table))
           100)))
  
  (testing "{a = 100; b = a + 1; b} == 101"
    (ok (= (evaluate (t-seq (t-assign 'a
                                      (t-int 100))
                            (t-assign 'b
                                      (t-add (t-id 'a)
                                             (t-int 1)))
                            (t-id 'b))
                     (make-hash-table))
           101))))

(deftest check-evaluating-if
  (testing "(if(1 < 2) 2 else 1) == 2"
    (ok (= (evaluate (t-if (t-lt (t-int 1)
                                 (t-int 2))
                           (t-int 2)
                           (t-int 1))
                     (make-hash-table))
           2)))
  
  (testing "(if(1 > 2) 2 else 1) == 1"
    (ok (= (evaluate (t-if (t-gt (t-int 1)
                                 (t-int 2))
                           (t-int 2)
                           (t-int 1))
                     (make-hash-table))
           1)))
  
  (testing (format nil
                   "{ ~
                      a = 100; ~
                      b = 200; ~
                      if (a < b) { ~
                        500; ~
                      } else { ~
                        1000; ~
                    }")
    (ok (= (evaluate (t-seq (t-assign 'a
                                      (t-int 100))
                            (t-assign 'b
                                      (t-int 200))
                            (t-if (t-lt (t-id 'a)
                                        (t-id 'b))
                                  (t-int 500)
                                  (t-int 1000)))
                     (make-hash-table))
           500))))

(deftest check-evaluating-program
  (testing (format nil
                   "func add(a, b) { ~
                      return a + b; ~
                    } ~
                    add(1, 2); ~
                   ")
    (ok (= (evaluate (t-program
                      (list (t-func 'add
                                    '(a b)
                                    (t-add (t-id 'a)
                                           (t-id 'b))))
                      (t-call 'add
                              (t-int 1)
                              (t-int 2)))
                     (make-hash-table))
           3)))
  (testing (format nil
                   "i = 0; ~
                    while (i < 10) { ~
                      i = i + 1; ~
                    } ~
                    i ~
                   ")
    (ok (= (evaluate (t-program
                      '()
                      (t-assign 'i
                                (t-int 0))
                      (t-while (t-lt (t-id 'i)
                                     (t-int 10))
                               (t-assign 'i
                                         (t-add (t-id 'i)
                                                (t-int 1))))
                      (t-id 'i))
                     (make-hash-table))
           10))))
