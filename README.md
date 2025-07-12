# `ucond`

[![Test](https://github.com/hawnzug/ucond/actions/workflows/test.yml/badge.svg)](https://github.com/hawnzug/ucond/actions/workflows/test.yml)

## Overview

This package provides two macros, `ucond` and `ucase`,
which extend the built-in `cond` and `pcase` with more flexible control flows:
- Mutually nested and interleaved `ucond` and `ucase`, where inner constructs can fall through to the outer level.

  ``` elisp
  (ucase (list 1 2 3 4)         ; Start a pattern matching like pcase.
    (`(1 2 . ,rest)             ; Matched, then
     :and-ucase (length rest)   ; start a nested pattern matching.
     (0 'error-1)               ; Not matched. Move next.
     (1 'error-2))              ; Not matched. Fall through to outer ucase.
    (`(1 . ,_) 'yes))           ; Match. Return 'yes.
  ```
- Interleaved `let*`,
  which works like `pcase-let*` with an otherwise branch and an option to fall through to the outer level.

  ``` elisp
  (ucond                                ; Start a multi-way if like cond.
   (let* ((x 1) (y (1+ x))))            ; Bind x and y.
   ((> x y) 'error-1)
   (let* (`(,a ,b ,c ,d) (list x y))    ; Failed to bind a, b, c, d,
     :otherwise (error-2 x y))          ; return the :otherwise branch.
   ((< x y) 0))
  ```

## Motivation

The primary motivation for `ucond` and `ucase` is to flatten nested code
while maintaining or even improving the program's readability and clarity.
Here are some quick comparisons to see how `ucond` and `ucase` flatten programs.

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
       (_ 'error-2)))    ; Error handling far away from pattern,
    (_ 'error-1))        ; and written in reversed order.
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
         (_ (fallback))))    ; Cannot jump to the next clause
      (_ (fallback))))       ; in the outer pcase directly.
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

These examples should give you a feel for how to use `ucond` and `ucase` in practice.
If you just need a quick reference, see the built-in docstrings for `ucond` and `ucase`.
For a deep dive into the core mechanics, read the [Complete Guide](#complete-guide) below.

## Complete Guide

### Overall Structure and Control Flow

As shown in the examples above, `ucond` and `ucase` look like the built-in constructs `cond` and `pcase`:
``` elisp
(ucond CLAUSE...)
(ucase EXPR CLAUSE...)
```
The key difference is that
there are special clauses which can introduce nested `ucond` and `ucase`,
forming a single tree-like conditional structure.
For example:
``` elisp
(ucond                                 ; A nesting can be introduced by:
 (ucase EXPR CLAUSE...)                ; the ucase clause,
 (CONDITION :and-ucase EXPR CLAUSE...) ; the :and-ucase keyword,
 (ucond CLAUSE...)                     ; the ucond clause,
 (CONDITION :and-ucond CLAUSE...))     ; the :and-ucond keyword.
```
It is important to distinguish this single nested tree from a standard nested form:
``` elisp
(ucond
 (CONDITION (ucond CLAUSE...))) ; This is not ucond's nesting.
```
This is not a single tree but two separate `ucond`.
The inner `ucond` is the body of the `CONDITION` clause, not a sub-clause of the outer `ucond`.
Their macro expansions are independent.

There are three basic control-flow rules of `ucond` and `ucase`:
1. **Top-to-Bottom**: Just like `cond` and `pcase`, clauses are executed sequentially.
1. **First-Success**: Only the first clause that successfully runs its body will *return* a value,
   which becomes the value of the entire top-level construct.
1. **Fall-Through**: When the control flow reaches the end of an inner block,
   it falls through to the next clause in the parent block.
   If it is alreadly at the outmost block, the entire construct evaluates to `nil`.

The first two rules are the same in `cond` and `pcase`.
The third "Fall Through" rule is a natural generalization of the "Top-to-Bottom" execution in a nested setting.

### Primitive Clauses

All the clauses available in `ucond` and `ucase` can be built upon two primitives,
`let*` and `match*`, which provide opposite control flows.

The `let*` clause works like a guard.
The subsequent clauses will only be executed when pattern matchings in `let*` succeed.

Syntax: `(let* ((PATTERN EXPR)...) [:otherwise BODY...])`, where the `:otherwise ...` branch is optional.
- If all patterns match, the control flow moves to the next clause,
  and the bindings are made available to all subsequent clauses
  within the current `ucond` or `ucase`.
- If any pattern fails to match:
  - If the `:otherwise` branch is non-empty, return the value of `BODY...`.
  - If there is no `:otherwise` branch, or `BODY` is empty, end the current `ucond` or `ucase` and fall through to the parent block.

The `match*` clause works like a standard case in `pcase`,
which executes its body when the pattern matchings succeed.

Syntax: `(match* ((PATTERN EXPR)...) BODY...)`.
- If any pattern fails to match, the control flow moves to the next clause after `match*`.
- If all patterns match, make the bindings available to `BODY...` and run it.
  `BODY...` has three variations:
  - If `BODY...` is `:and-ucond CLAUSES...`, start a nested `ucond` with `CLAUSES`.
  - If `BODY...` is `:and-ucase EXPR-1 CLAUSES...`, start a nested `ucase` with the expression `EXPR-1` and `CLAUSES`.
  - Otherwise, return `BODY...`.

The flexibility of `ucond` and `ucase` comes from combining these opposite behaviors.
| Condition | Control Flow of `let*` | Control Flow of `match*` |
| :--- | :--- | :--- |
| Patterns Match | To next clause (with new bindings) | To body (with new bindings) |
| Patterns Fail  | To `:otherwise` (or fall through) | To next clause |

### Derivable Clauses

All other clauses can be derived from either `let*` or `match*`.

Clauses based on `let*` (`OTHERWISE` represents the optional `:otherwise ELSE...` branch):
- `(when-let* ((VAR EXPR)...) OTHERWISE)`: A `let*` that also checks the bound variables are non-nil.
  Analog to the built-in `when-let*`.
  - Equivalent to `(let* (((and VAR (guard VAR)) EXPR)...) OTHERWISE)`.
- `(when CONDITION OTHERWISE)`: A `let*` that only checks one condition is non-nil.
  Analog to the built-in `when`.
  - Equivalent to `(let* (((guard CONDITION) t)) OTHERWISE)`.

Clauses based on `match*` (`BODY...` can begin with `:and-ucond` or `:and-ucase` to start nesting):
- `(ucond CLAUSES)`: Starts a nested `ucond` block.
  - Equivalent to `(match* ((_ t)) :and-ucond CLAUSES)`.
- `(ucase EXPR CLAUSES)`: Starts a nested `ucase` block.
  - Equivalent to `(match* ((_ t)) :and-ucase EXPR CLAUSES)`.
- In `ucond`, `(CONDITION BODY...)` is the standard `cond`-like clause.
  - Equivalent to `(match* (((guard CONDITION) t)) BODY...)`.
  - When `BODY...` is empty, as in `(CONDITION)`,
    the value of `CONDITION` is returned (only evaluated once).
    This is for compatibility with `cond`.
- In `ucase`, `(PATTERN BODY...)` is the standard `pcase`-like clause.
  - Equivalent to `(match* ((PATTERN VAL-EXPR)) BODY...)`,
    where `VAL-EXPR` is the value of the expression matched by `ucase`,
    that is, the first argument of `ucase`.

Below is a table summarizing all derivable clauses and their equivalent forms in `let*` or `match*`:
| Derivable Clause                        | Equivalent Form in Primitive Clause                  |
|-----------------------------------------|------------------------------------------------------|
| `(when-let* ((VAR EXPR)...) OTHERWISE)` | `(let* (((and VAR (guard VAR)) EXPR)...) OTHERWISE)` |
| `(when CONDITION OTHERWISE)`            | `(let* (((guard CONDITION) t)) OTHERWISE)`           |
| `(ucond CLAUSES)`                       | `(match* ((_ t)) :and-ucond CLAUSES)`                |
| `(ucase EXPR CLAUSES)`                  | `(match* ((_ t)) :and-ucase EXPR CLAUSES)`           |
| `(PATTERN BODY...)`                     | `(match* ((PATTERN EXPR)) BODY...)`                  |
| `(CONDITION BODY...)`                   | `(match* (((guard CONDITION) t)) BODY...)`           |
| `(CONDITION)`                           | `(match* (((and x (guard x)) CONDITION)) x)`         |

**Note**: The equivalent forms are only shown to clarify their semantics.
The actual implementation might differ.

## Similar Constructs in Other Languages

This section is intended for readers familiar with other languages,
and want to relate `ucond` and `ucase` to existing language features.
It is not necessary to read this section to learn how this package works,
and it might be full of obscure terminologies specific to particular languages.

The most distinctive feature of `ucond` and `ucase` is the interleaved `(let* ... :otherwise ...)` construct within multi-way if (`cond`) and pattern matching.
At first glance, this construct is similar to the [`let-else`](https://github.com/rust-lang/rfcs/blob/master/text/3137-let-else.md) in Rust
and the [`guard-let-else`](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/controlflow#Early-Exit) in Swift.
But they differ in important ways.

In Rust and Swift,
these constructs are only used within a sequential construct,
that is, a sequence of statements.
They are not interleaved in a branching construct like multi-way if or pattern matching.
Therefore their `else` blocks must always diverge, for example, return, break, or error.

On the other hand, the `let* :otherwise` clause in this package is used in a branching construct,
so it allows the `:otherwise` branch to contain any expressions or fall through.
It can be viewed as a generalization of `let-else` in a nested branching construct.

Without `:otherwise` in `let*`, this package is almost the same as
[The Ultimate Conditional Syntax](https://dl.acm.org/doi/10.1145/3689746) in MLscript,
if we ignore its ML-like and indentation sensitive syntax and focus on the abstract syntax.
In fact, this paper is a major inspiration of this package.

The nested and interleaved `ucond` and `ucase` with fall-throughs can be viewed as the nested guards in Haskell
(though not yet implemented), see
[these](https://stackoverflow.com/questions/28526768/is-there-in-haskell-something-similar-to-sub-guards)
[three](https://stackoverflow.com/questions/34124558/is-it-possible-to-nest-guards-in-haskell)
[questions](https://stackoverflow.com/questions/40334968/nested-guards-on-haskell) for examples.
Note that Haskell's chained guards and interleaved let-bindings in guards
are different from nested guards and interleaved `let*` between clauses.

OCaml does not support nested guards, but there is an extensive
[discussion](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600) about it,
proposing a syntax similar to `ucase`.

Scala also has a similar [proposal](https://github.com/scala/scala3/discussions/17728) for nested patterns in guards.

Agda's `with` is a clean syntax (but it's more than that)
for nested guards (on the left), but it has no fall-through behavior.

Racket does not support nested guards directly,
but can easily implement this feature using labelled branches ([`=> id`](https://docs.racket-lang.org/reference/match.html))
and manually escaping to labels.
It offers more freedom and works like a goto in a pattern matching.
This feature is also listed as a todo in the source code of `pcase`.

## Comparision with `cond*` and `pcase`

Emacs 31 added a new `cond*` construct which extends the traditional `cond`.
It supports "non-exit clause" which works similar to the `let*` clause in this package,
but it is non-exiting, so the control flow is more limited.
There is no nested guards with fall-through in `cond*`.
The `match*` keyword in this package is borrowed from `cond*`, but they have different behaviors.

The built-in `pcase` does not support interleaved variable bindings like `let*`.
However, it is actually possible to have nested guards with fall-through using the `or` pattern.
For example, the following nested `ucase`
``` elisp
(ucase (list 1 2 3 4)
  (`(1 . ,rest)
   :and-ucase (reverse rest)
   (`(2 ,x ,y) (+ x y))
   (`(3 ,z) (* z z)))
  (_ 'fallthrough))
```
is equivalent to this single `pcase`:
``` elisp
(pcase (list 1 2 3 4)
  (`(1 . ,(app reverse
               (or (and `(2 ,x ,y) (let ret (+ x y)))
                   (and `(3 ,z) (let ret (* z z))))))
   ret)
  (_ 'fallthrough))
```
Although it is more verbose, the logic still looks clear.
Scalability (more nesting and more branches) might be an issue,
and using the `or` pattern in this way has the problem of making all bindings available to the body.

## Caveats and Non-Goals

### Caveats
- Performance might degrade with deeply nested `ucond` and `ucase` that could fall through,
  roughly one `if` and `eq` per level, compared to a direct `goto`, which is not available in Elisp.
- In some cases, `cond` might be compiled to a jump table,
  but a direct translation to `ucond` might not be,
  which would make `ucond` slower.
- Some Emacs keywords like `let*` and `when` are overloaded in `ucond` and `ucase`, which might lead to confusion.
  Keywords like `:otherwise` might seem unfamiliar to Elisp users.
- ~~The byte-compiler might warn about a `pcase` pattern being shadowed.
  This will not affect correctness, but the issue should be fixed if possible.~~
  This package should no longer generate shadowed patterns.
  If you see such warnings and there are no shadowed patterns in your code,
  please open an issue.

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
