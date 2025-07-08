# `ucond`

[![Test](https://github.com/hawnzug/ucond/actions/workflows/test.yml/badge.svg)](https://github.com/hawnzug/ucond/actions/workflows/test.yml)

This package provides two macros, `ucond` and `ucase`,
which extend the built-in `cond` and `pcase` with more flexible control flows:
- Nested and interleaved `cond` and `pcase`, where inner constructs can fall through to outer levels.
- Interleaved `pcase-let*` with an else branch and an option to fall through.

The primary motivation for `ucond` and `ucase` is to flatten nested code
while maintaining or even improving the program's readability and clarity.
Let's look at some quick comparisons to see how `ucond` and `ucase` flatten programs,
before diving into the details of their syntax and semantics.

- **Before**: Nested `pcase` and `cond` for destructuring bindings and comprehensive error handling.
  ``` elisp
  (pcase (get-a-pair)
    (`(,x1 . ,x2)
     (pcase (get-status x1 x2)
       (`(:status 200 :data ,x3)
        (cond
         ((test-a x1 x2 x3) 'a)
         ((test-b x1) 'b)
         (t 'error-3)))
       (_ 'error-2)))
    (_ 'error-1))
  ```
  **After**: Flattened by `ucond` and interleaved `let*` with early returns.
  ``` elisp
  (ucond
   (let* ((`(,x1 . ,x2) (get-a-pair)))
     :otherwise 'error-1)
   (let* ((`(:status 200 :data ,x3) (get-status x1 x2)))
     :otherwise 'error-2)
   ((test-a x1 x2 x3) 'a)
   ((test-b x1) 'b)
   (t 'error-3))
  ```
- **Before**: Nested `pcase` matching the same expression to delay a heavy computation.
  ``` elisp
  (pcase (get-response)
    (`(:failure ,err) (report-error err))
    (`(:simple ,x) x)
    (response
     (let ((num (heavy-computation)))        ; Compute the pattern.
       (pcase response                       ; Continue matching.
         (`(:ok ,(pred (< num)) ,x) x)                  ; Pattern used here
         (`(:double ,(pred (> num)) ,x) (* 2 x))        ; and here.
         (_ 'error-a)))))
  ```
  **After**: Flattened by `ucase` with interleaved `let*`.
  ``` elisp
  (ucase (get-response)
    (`(:failure ,err) (report-error err))
    (`(:simple ,x) x)
    (let* ((num (heavy-computation))))
    (`(:ok ,(pred (< num)) ,x) x)
    (`(:double ,(pred (> num)) ,x) (* 2 x))
    (_ 'error-a))
  ```
- **Before**: Nested `pcase` and a local function to handle common fallback logic.
  ``` elisp
  (cl-flet ((fallback () (verbose-fallback) (clean-up) (report-error)))
    (pcase expr1
      (`(ok ,x1)
       (pcase (compute-something x1)
         (`(case1 ,x2) (f1 x1 x2))
         (`(case2 ,x2 ,x3) (f2 x1 x2 x3))
         (_ (fallback))))
      (_ (fallback))))
  ```
  **After**: Simplified by nested `ucase` and `:and-ucase` with fall-through.
  ``` elisp
  (ucase expr1
    (`(ok ,x1)
     :and-ucase (compute-something x1)
     (`(case1 ,x2) (f1 x1 x2))
     (`(case2 ,x2 ,x3) (f2 x1 x2 x3)))
    (_ (verbose-fallback) (clean-up) (report-error)))
  ```

## Installation

> [!WARNING]
> This package is still in alpha stage and is subject to major syntax changes.
> Do not use it in production until it is on MELPA.

`use-package` with `package-vc`:
``` elisp
(use-package ucond
  :vc (:url "https://github.com/hawnzug/ucond.git" :rev :newest))
```

`use-package` with `elpaca`:
``` elisp
(use-package ucond
  :ensure (:host github :repo "hawnzug/ucond"))
```

Or manually download `ucond.el`, add it to your `load-path`, and `(require 'ucond)`.

## Quick Start

The macros `ucond` and `ucase` can be used as drop-in replacements
for the built-in `cond` and `pcase` in most situations:
``` elisp
(ucond
 (condition-1 body-1...)
 (condition-2 body-2...)
 (condition-3 body-3...))

(ucase expr
  (pattern-1 body-1...)
  (pattern-2 body-2...)
  (pattern-3 body-3...))
```
In addition to default clauses like `(condition body...)` and `(pattern body...)`,
`ucond` and `ucase` also support special clauses like `(let* ...)` to allow for more flexible control flows.
In this quick start, we will introduce several commonly used special clauses.

### `let*`: Local Destructuring Bindings with Early Returns

We can add local bindings between clauses with the special `let*` clause:
``` elisp
(ucond
 (let* ((x 1) (y 2)))
 (condition-1 (+ x y))
 (let* ((z (+ x y)) (w (1+ z))))
 (condition-2 (+ x y z w)))

(ucase expr
  (let* ((x 1) (y 2)))
  (pattern-1 (+ x y))
  (let* ((z (+ x y)) (w (1+ z))))
  (pattern-2 (+ x y z w)))
```
Most special clauses like `let*` work the same in both `ucond` and `ucase`.
Therefore, the rest of this quick start will only show examples in `ucond`.

The `let*` clause is not the built-in Elisp special form.
It is overloaded in `ucond` and `ucase` but has a similar syntax.
The `let*` clause actually works more like `pcase-let*`,
so we can do pattern matching and destructuring bindings within it:
``` elisp
(ucond
 (let* ((`(,x ,y ,z) (list 1 2 3))))
 ((< x y z) (+ x y z)))        ; => Returns 6
```
When pattern matching fails in a `let*` clause,
we can use the `:otherwise` keyword to specify the return value:
``` elisp
(ucond
 (let* ((`(,x ,y) (list 1 2 3)))
   :otherwise 42)        ; => Returns 42
 (t (+ x y)))
```

### Nested and Interleaved `ucond` and `ucase` with Fall-Through

We can introduce nested and interleaved `ucond` and `ucase`
using the special `ucond` and `ucase` clauses:
``` elisp
(ucond
 (nil 0)
 (ucase (+ 1 1)
   (0 1)
   (2 2))        ; => Returns 2
 (t 3))
```
When all the clauses are exhausted in an inner `ucond` or `ucase`,
the control flow will fall through to the next clause in the outer level:
``` elisp
(ucond
 (nil 0)
 (ucase (+ 1 1)
   (0 1)
   (100 2))    ; Fall through
 (nil -1)      ; Examined
 (t 3))        ; => Returns 3
```

The `let*` clause can also lead to fall-through
when pattern matching fails and there is no `:otherwise` branch:
``` elisp
(ucond
 (nil 0)
 (ucase (+ 1 1)
   (let* ((`(,x ,y) (list 1 2 3))))    ; Fall through
   (_ (+ x y)))                        ; Skipped
 (t 3))                                ; => Returns 3
```

If a fall-through occurs in the outermost level,
the entire construct returns `nil`:
``` elisp
(ucond (nil 1)) ; Fall through
; Returns nil

(ucond
 (let* ((`(,x ,y) (list 1 2 3))))    ; Fall through
 (t (+ x y)))
; Returns nil
```
This behavior coincides with `cond` and `pcase`
when their clauses are exhausted.

Nested `ucond` and `ucase` can also be introduced with the
`:and-ucond` and `:and-ucase` keywords,
immediately after a condition in `ucond`
or a pattern in `ucase`:
``` elisp
(ucond
 (condition-1
  :and-ucond
  (condition-2 body-1)
  (condition-3
   :and-ucase expr-1
   (pattern-1 body-2)
   (pattern-2
    :and-ucond
    (condition-4 body-3)))))
```
They work like a directly nested `ucond` (or `ucase`)
with a pre-condition (or pre-pattern),
and the fall-through behavior is the same.

These examples should cover the common uses of `ucond` and `ucase`.
For a complete reference, see the docstrings of `ucond` and `ucase`,
or read the section [Full Reference](#full-reference).

## Full Reference

## Comparisons

## Caveats and Non-Goals

### Caveats
- Performance might degrade with deeply nested `ucond` and `ucase` that could fall through,
  roughly one `if` and `eq` per level, compared to a direct `goto`, which is not available in Elisp.
- The byte-compiler might warn about a `pcase` pattern being shadowed.
  This will not affect correctness, but the issue should be fixed if possible.
- In some cases, `cond` might be compiled to a jump table,
  but a direct translation to `ucond` might not be,
  which would make `ucond` slower.
- Some Emacs keywords like `let*` and `when` are overloaded in `ucond` and `ucase`, which might lead to confusion.
  Keywords like `:otherwise` might seem unfamiliar to Elisp users.

### Non-Goals

This package does **not** provide:
- A new pattern language. It uses `pcase` under the hood.
- A loop construct. It is not `cl-loop` and has nothing to do with loops.
- A new sequential construct.
  Ultimately, `ucond` and `ucase` are conditional constructs that form a tree-like structure,
  and they will choose one branch to execute.
  It might be tempting to define a macro like `ucond-defun`:
  ``` elisp
  (ucond-defun func (args...)
    "Docstring"
    (let* ... :otherwise ...)
    (let* ... :otherwise ...)
    (ucase ...)
    (ucond ...)
    BODY...)
  ```
  Although this macro looks convenient,
  it is not recommended and is considered an abuse of `ucond`.

## Acknowledgments

- [The Ultimate Conditional Syntax](https://dl.acm.org/doi/10.1145/3689746):
  The 'u' in `ucond` and `ucase` comes from this paper.
  This package is basically the ultimate conditional syntax,
  plus the special `(let* ... :otherwise ...)` clause.
- ["... We all want it, ..."](https://stackoverflow.com/a/34168666),
  an answer to the StackOverflow question "Is it possible to nest guards in Haskell?".
  Although I'm not sure if "we" really all want it, at least I do.
- [Rust `let else` RFC](https://github.com/rust-lang/rfcs/blob/master/text/3137-let-else.md):
  It seems that many Rust users like this feature.
- [Tail cascade: a new indentation style for some OCaml constructs](https://discuss.ocaml.org/t/tail-cascade-a-new-indentation-style-for-some-ocaml-constructs/4736):
  This post advocates for a style that linearizes cascading tail pattern-matchings to avoid cascading indentations.
- Elisp built-in `pcase`: This is what `ucond` and `ucase` expand to.
- Elisp built-in `cond*`: It inspired me to unify the syntax of `ucond` and `ucase`.
  The keyword `match*` also comes from `cond*`.
