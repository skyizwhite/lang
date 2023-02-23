# Lang

Very simple interpreter written in Common Lisp.

Original(written in JavaScript): https://github.com/kmizu/minis

⚠️ This is a toy program for study.

## Requirements

- [roswell](https://github.com/roswell/roswell)
  - sbcl-bin/2.3.0
- [qlot](https://github.com/fukamachi/qlot)
- [rove](https://github.com/fukamachi/rove)
- [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria.git)
- [cl-annot-revisit](https://github.com/y2q-actionman/cl-annot-revisit)

## Setup

```zsh
$ ros install qlot
$ qlot install
```

## Usage

### Run program

```lisp
(ql:quickload :lang)
(in-package :lang)

(defparameter *env* (make-hash-table))

; Compute the factorial of 6 with a recursive function
(evaluate (t-program
           (list
            (t-func 'fact '(n)
                    (t-if (t-eq (t-id 'n) (t-int 0))
                          (t-int 1)
                          (t-mul (t-id 'n)
                                 (t-call 'fact
                                         (t-sub (t-id 'n)
                                                (t-int 1)))))))
           (t-call 'fact (t-int 6)))
          *env*)
; => 720
```

### Define your own AST node

```lisp
; Define AST node
(defnode foo
  :props (ch1 ch2) ; named child nodes
  :rest bodies)    ; variable-length child nodes
```
`defnode` macro will generate:
- `l-foo`: structure
- `l-foo-ch1`, `l-foo-ch2`, `l-foo-bodies`: accessor of structure
- `(t-foo (ch1 ch2 &rest bodies) ...)`: constructor of structure
- Other structure related methods

Then you can implement `evaluate` method to define how to evaluate the AST node.

```lisp
(defmethod evaluate ((e l-foo) env)
  ...)
```

## Run test

```zsh
$ .qlot/bin/rove tests/macros.lisp
$ .qlot/bin/rove tests/evaluator.lisp
```

<details>
<summary>Example</summary>

```
$ .qlot/bin/rove tests/evaluator.lisp

Testing System lang-tests

;; testing 'lang-tests/evaluator'
check-evaluating-bin-expr
  1 + 1 == 2
    ✓ Expect (= (EVALUATE (T-ADD (T-INT 1) (T-INT 1)) (MAKE-HASH-TABLE)) 2) to be true.
  1 - 2 == -1
    ✓ Expect (= (EVALUATE (T-SUB (T-INT 1) (T-INT 2)) (MAKE-HASH-TABLE)) -1) to be true.
  2 * 3 == 6
    ✓ Expect (= (EVALUATE (T-MUL (T-INT 2) (T-INT 3)) (MAKE-HASH-TABLE)) 6) to be true.
  6 / 2 == 3
    ✓ Expect (= (EVALUATE (T-DIV (T-INT 6) (T-INT 2)) (MAKE-HASH-TABLE)) 3) to be true.
  1 / 0 == Error!
  (1 + (2 * 3) - 1) / 2 == 3
    ✓ Expect (=
              (EVALUATE
               (T-DIV (T-SUB (T-ADD (T-INT 1) (T-MUL (T-INT 2) (T-INT 3))) (T-INT 1))
                      (T-INT 2))
               (MAKE-HASH-TABLE))
              3) to be true.
  1 < 2 == 1
    ✓ Expect (EVALUATE (T-LT (T-INT 1) (T-INT 2)) (MAKE-HASH-TABLE)) to be true.
  2 > 1 == 1
    ✓ Expect (EVALUATE (T-GT (T-INT 2) (T-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 <= 1 == 1
    ✓ Expect (EVALUATE (T-GTE (T-INT 1) (T-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 >= 1 == 1
    ✓ Expect (EVALUATE (T-LTE (T-INT 1) (T-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 == 1 == 1
    ✓ Expect (EVALUATE (T-EQ (T-INT 1) (T-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 != 0 == 1
    ✓ Expect (EVALUATE (T-NE (T-INT 1) (T-INT 0)) (MAKE-HASH-TABLE)) to be true.
check-evaluating-assignment
  {a = 100; a} == 100
    ✓ Expect (=
              (EVALUATE (T-SEQ (T-ASSIGN 'A (T-INT 100)) (T-ID 'A))
                        (MAKE-HASH-TABLE))
              100) to be true.
  {a = 100; b = a + 1; b} == 101
    ✓ Expect (=
              (EVALUATE
               (T-SEQ (T-ASSIGN 'A (T-INT 100))
                      (T-ASSIGN 'B (T-ADD (T-ID 'A) (T-INT 1))) (T-ID 'B))
               (MAKE-HASH-TABLE))
              101) to be true.
check-evaluating-if
  (if(1 < 2) 2 else 1) == 2
    ✓ Expect (=
              (EVALUATE (T-IF (T-LT (T-INT 1) (T-INT 2)) (T-INT 2) (T-INT 1))
                        (MAKE-HASH-TABLE))
              2) to be true.
  (if(1 > 2) 2 else 1) == 1
    ✓ Expect (=
              (EVALUATE (T-IF (T-GT (T-INT 1) (T-INT 2)) (T-INT 2) (T-INT 1))
                        (MAKE-HASH-TABLE))
              1) to be true.
  { a = 100; b = 200; if (a < b) { 500; } else { 1000; }
    ✓ Expect (=
              (EVALUATE
               (T-SEQ (T-ASSIGN 'A (T-INT 100)) (T-ASSIGN 'B (T-INT 200))
                      (T-IF (T-LT (T-ID 'A) (T-ID 'B)) (T-INT 500) (T-INT 1000)))
               (MAKE-HASH-TABLE))
              500) to be true.
check-evaluating-program
  func add(a, b) { return a + b; } add(1, 2);
    ✓ Expect (=
              (EVALUATE
               (T-PROGRAM (LIST (T-FUNC 'ADD '(A B) (T-ADD (T-ID 'A) (T-ID 'B))))
                          (T-CALL 'ADD (T-INT 1) (T-INT 2)))
               (MAKE-HASH-TABLE))
              3) to be true.
  i = 0; while (i < 10) { i = i + 1; } i
    ✓ Expect (=
              (EVALUATE
               (T-PROGRAM 'NIL (T-ASSIGN 'I (T-INT 0))
                          (T-WHILE (T-LT (T-ID 'I) (T-INT 10))
                                   (T-ASSIGN 'I (T-ADD (T-ID 'I) (T-INT 1))))
                          (T-ID 'I))
               (MAKE-HASH-TABLE))
              10) to be true.
              
✓ 1 test completed

Summary:
  All 1 test passed.
```
</details>
