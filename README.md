# Lang

Very simple interpreter written in Common Lisp.

Original(written in JavaScript): https://github.com/kmizu/minis

⚠️ This is a toy program for study.

## Requirements

- [roswell](https://github.com/roswell/roswell)
  - sbcl-bin/2.3.0
- [qlot](https://github.com/fukamachi/qlot)
- [rove](https://github.com/fukamachi/rove)
- [lake](https://github.com/takagi/lake)
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
$ .qlot/bin/lake spec
```
