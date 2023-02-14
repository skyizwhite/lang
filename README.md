# Lang

Very simple interpreter written in Common Lisp.

Original(written in JavaScript): https://github.com/kmizu/minis

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

## Run test

```zsh
$ .qlot/bin/rove tests/evaluator.lisp
```

<details>
<summary>Example</summary>

```
$ .qlot/bin/rove tests/evaluator.lisp

Testing System lang-tests/evaluator

;; testing 'lang-tests/evaluator'
check-evaluating-bin-expr
  1 + 1 == 2
    ✓ Expect (= (EVALUATE (MAKE-ADD (MAKE-INT 1) (MAKE-INT 1)) (MAKE-HASH-TABLE)) 2) to be true.
  1 - 2 == -1
    ✓ Expect (= (EVALUATE (MAKE-SUB (MAKE-INT 1) (MAKE-INT 2)) (MAKE-HASH-TABLE)) -1) to be true.
  2 * 3 == 6
    ✓ Expect (= (EVALUATE (MAKE-MUL (MAKE-INT 2) (MAKE-INT 3)) (MAKE-HASH-TABLE)) 6) to be true.
  6 / 2 == 3
    ✓ Expect (= (EVALUATE (MAKE-DIV (MAKE-INT 6) (MAKE-INT 2)) (MAKE-HASH-TABLE)) 3) to be true.
  1 / 0 == Error!
  (1 + (2 * 3) - 1) / 2 == 3
    ✓ Expect (=
              (EVALUATE
               (MAKE-DIV
                (MAKE-SUB
                 (MAKE-ADD (MAKE-INT 1) (MAKE-MUL (MAKE-INT 2) (MAKE-INT 3)))
                 (MAKE-INT 1))
                (MAKE-INT 2))
               (MAKE-HASH-TABLE))
              3) to be true.
  1 < 2 == 1
    ✓ Expect (EVALUATE (MAKE-LT (MAKE-INT 1) (MAKE-INT 2)) (MAKE-HASH-TABLE)) to be true.
  2 > 1 == 1
    ✓ Expect (EVALUATE (MAKE-GT (MAKE-INT 2) (MAKE-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 <= 1 == 1
    ✓ Expect (EVALUATE (MAKE-GTE (MAKE-INT 1) (MAKE-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 >= 1 == 1
    ✓ Expect (EVALUATE (MAKE-LTE (MAKE-INT 1) (MAKE-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 == 1 == 1
    ✓ Expect (EVALUATE (MAKE-EQ (MAKE-INT 1) (MAKE-INT 1)) (MAKE-HASH-TABLE)) to be true.
  1 !=e 0 == 1
    ✓ Expect (EVALUATE (MAKE-NE (MAKE-INT 1) (MAKE-INT 0)) (MAKE-HASH-TABLE)) to be true.
check-evaluating-assignment
  {a = 100; a} == 100
    ✓ Expect (=
              (EVALUATE (MAKE-SEQ (MAKE-ASSIGN 'A (MAKE-INT 100)) (MAKE-ID 'A))
                        (MAKE-HASH-TABLE))
              100) to be true.
  {a = 100; b = a + 1; b} == 101
    ✓ Expect (=
              (EVALUATE
               (MAKE-SEQ (MAKE-ASSIGN 'A (MAKE-INT 100))
                         (MAKE-ASSIGN 'B (MAKE-ADD (MAKE-ID 'A) (MAKE-INT 1)))
                         (MAKE-ID 'B))
               (MAKE-HASH-TABLE))
              101) to be true.
check-evaluating-if
  (if(1 < 2) 2 else 1) == 2
    ✓ Expect (=
              (EVALUATE
               (MAKE-IF (MAKE-LT (MAKE-INT 1) (MAKE-INT 2)) (MAKE-INT 2)
                        (MAKE-INT 1))
               (MAKE-HASH-TABLE))
              2) to be true.
  (if(1 > 2) 2 else 1) == 1
    ✓ Expect (=
              (EVALUATE
               (MAKE-IF (MAKE-GT (MAKE-INT 1) (MAKE-INT 2)) (MAKE-INT 2)
                        (MAKE-INT 1))
               (MAKE-HASH-TABLE))
              1) to be true.
  { a = 100; b = 200; if (a < b) { 500; } else { 1000; }
    ✓ Expect (=
              (EVALUATE
               (MAKE-SEQ (MAKE-ASSIGN 'A (MAKE-INT 100))
                         (MAKE-ASSIGN 'B (MAKE-INT 200))
                         (MAKE-IF (MAKE-LT (MAKE-ID 'A) (MAKE-ID 'B)) (MAKE-INT 500)
                                  (MAKE-INT 1000)))
               (MAKE-HASH-TABLE))
              500) to be true.
check-evaluating-program
  func add(a, b) { return a + b; } add(1, 2);
    ✓ Expect (=
              (EVALUATE
               (MAKE-PROGRAM
                (LIST (MAKE-FUNC 'ADD '(A B) (MAKE-ADD (MAKE-ID 'A) (MAKE-ID 'B))))
                (MAKE-CALL 'ADD (MAKE-INT 1) (MAKE-INT 2)))
               (MAKE-HASH-TABLE))
              3) to be true.
  i = 0; while (i < 10) { i = i + 1; } i
    ✓ Expect (=
              (EVALUATE
               (MAKE-PROGRAM 'NIL (MAKE-ASSIGN 'I (MAKE-INT 0))
                             (MAKE-WHILE (MAKE-LT (MAKE-ID 'I) (MAKE-INT 10))
                                         (MAKE-ASSIGN 'I
                                                      (MAKE-ADD (MAKE-ID 'I)
                                                                (MAKE-INT 1))))
                             (MAKE-ID 'I))
               (MAKE-HASH-TABLE))
              10) to be true.

✓ 1 test completed

Summary:
  All 1 test passed.
```
</details>
