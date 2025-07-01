;;; ucond.el --- Ultimate Conditional Syntax with let-else  -*- lexical-binding: t -*-

;; Copyright (C) 2025  Zhuyang Wang

;; Author: Zhuyang Wang <hawnzug@gmail.com>
;; Created: 2025-06-29
;; Keywords: macros
;; Version: 0.1.0

;;; Commentary:

;; Combine the Ultimate Conditional Syntax from MLscript
;; (see https://doi.org/10.1145/3689746)
;; with let-else, using pcase.

;;; Code:

;;;; Core

(defmacro ucond--core (&rest cases)
  "The core language of `ucond'.
CASES is a list of cases. Each case can take one of the forms:

1. (c:then PATTERN EXPR THEN-BODY...).
2. (c:else PATTERN EXPR ELSE-BODY...).
3. (c:cond PATTERN EXPR NESTED-CASES).

Every CASE has a PATTERN and a EXPR,
and it will try to match PATTERN with EXPR,
using the `pcase' pattern language,
as in (pcase EXPR (PATTERN THEN...) (_ ELSE...)).
The difference is the control flow.

In the c:then form, if PATTERN matches EXPR, execute THEN-BODY...,
and the rest CASES will be skipped.
If PATTERN does not match EXPR, move on to the next CASE.
This form works like a normal clause in `pcase'.

In the c:else form, if PATTERN matches EXPR,
it will continue to check the rest CASES,
with the bindings in PATTERN available to the rest CASES.
If PATTERN does not match EXPR, execute ELSE-BODY...,
and the rest CASES will be skipped.
This form is the dual of the c:then form.
The control flows are flipped.
It works like the combination of bind* and pcase* clause in `cond*',
with the additional ELSE-BODY for the failed match.

In the c:cond form, if PATTERN matches EXPR,
start a nested `ucond--core' on NESTED-CASES at the inner level.
The NESTED-CASES have the same forms of CASES.
The execution of NESTED-CASES is also the same,
except when all NESTED-CASES failed to match,
the control will return to the outer level,
that is, check the next CASE after this c:cond form.
In other words, this is a nested match
which falls through to the outer match when no inner CASE matches.

The `ucond--core' construct can be translated to `pcase' in a direct way,
to help understand its semantics.
The translation is defined inductively on CASES.
Each case introduces a new nested layer of `pcase'.

The form (c:then PATTERN EXPR THEN-BODY...) can be translated to

  (pcase EXPR
    (PATTERN THEN-BODY...)
    (_ REST))

where REST is (translate CASES FALL-THROUGH),
that is, the translation of the rest CASES
with potential outer FALL-THROUGH cases.

The form (c:else PATTERN EXPR ELSE-BODY...) can be translated to

  (pcase EXPR
    (PATTERN REST)
    (_ ELSE-BODY...))

where REST is the same as in the previous case.
It is easy to see the bindings introduced by PATTERN
are available to the rest cases.

The form (c:cond PATTERN EXPR NESTED-CASES...) can be translated to

  (pcase EXPR
    (PATTERN (translate NESTED-CASES REST))
    (_ REST))

where REST is the same as in the previous case.
The REST cases become the fall-through cases
in the translation of NESTED-CASES."
  (declare (indent nil))
  (ucond--core-expand cases nil))

(defun ucond--core-expand (cases fallback)
  "Expand CASES, fall through to FALLBACK.
The code after expansion should behaves the same as the one
translated by the algorithm in the documentation of `ucond--core'."
  (if (null cases) fallback
    (let ((rest (ucond--core-expand (cdr cases) fallback)))
      (pcase (car cases)
        (`(c:then ,pattern ,expr . ,then)
         `(pcase ,expr
            (,pattern ,@then)
            (_ ,rest)))
        (`(c:else ,pattern ,expr . ,else)
         `(pcase ,expr
            (,pattern ,rest)
            (_ ,@else)))
        (`(c:cond ,pattern ,expr . ,nested-cases)
         `(pcase ,expr
            (,pattern ,(ucond--core-expand nested-cases rest))
            (_ ,rest)))))))

;;;; Multiple bindings

(defmacro ucond--bindings (&rest cases)
  "Allow multiple bindings for CASES in `ucond-core'."
  (cons 'ucond--core (ucond--bindings-expand cases)))

(defun ucond--bindings-expand (cases)
  (when cases
    (let ((rest (ucond--bindings-expand (cdr cases))))
      (pcase (car cases)
        (`(b:then ,bindings . ,then)
         (cons (ucond--bindings-case-expand bindings 'c:then then)
               rest))
        (`(b:cond ,bindings . ,nested-cases)
         (cons (ucond--bindings-case-expand
                bindings 'c:cond
                (ucond--bindings-expand nested-cases))
               rest))
        (`(b:else ,bindings . ,else)
         (append
          (cl-loop
           for binding in bindings
           collect (pcase binding
                     (`(,pattern ,expr)
                      `(c:else ,pattern ,expr ,@else))
                     (_ (error "Unknown binding"))))
          rest))))))

(defun ucond--bindings-case-expand (bindings end code)
  "Expand BINDINGS to and-cond followed by END CODE."
  (pcase bindings
    (`((,pattern ,expr))
     `(,end ,pattern ,expr ,@code))
    (`((,pattern ,expr) . ,rest-bindings)
     `(c:cond
       ,pattern ,expr
       ,(ucond--bindings-case-expand rest-bindings end code)))
    (_ (error "Unknown binding"))))

;;;; Sugar

(defmacro ucond (&rest clauses)
  "TODO: Documentation."
  (cons 'ucond--bindings
        (ucond--clauses-expand clauses)))

(defun ucond--clauses-expand (clauses)
  (mapcar #'ucond--clause-desugar clauses))

(defun ucond--clause-desugar (clause)
  (pcase clause
    (`(let* . ,rest) (ucond--clause-let*-desugar rest))
    (`(match* ,bindings . ,rest)
     (ucond--clause-and-desugar bindings rest))
    (`(when ,condition . ,rest)
     (ucond--clause-and-desugar `(((guard ,condition) nil)) rest))
    (`(ucase ,expr . ,rest)
     (ucond--clause-and-desugar
      '((_ t)) `(:and-ucase ,expr ,@rest)))
    (`(ucond . ,rest)
     (ucond--clause-and-desugar
      '((_ t)) `(:and-ucond ,@rest)))
    (`(_ . ,rest) `(b:then ((_ nil)) ,@rest))
    (_ (error "Unknown clause"))))

(defun ucond--clause-let*-desugar (rest)
  ;; Unfortunately we cannot use ucond now.
  (pcase rest
    (`(,bindings . ,rest)
     (let ((else (pcase rest
                   ('nil nil)
                   (`(:otherwise . ,else) else)
                   (_ (error "Missing :otherwise in let")))))
       `(b:else ,bindings ,@else)))
    (_ (error "Unknown let* clause"))))

(defun ucond--clause-and-desugar (bindings rest)
  (pcase rest
    (`(:and-ucond . ,cases)
     `(b:cond ,bindings ,@(ucond--clauses-expand cases)))
    (`(:and-ucase ,expr . ,cases)
     `(b:cond ,bindings
                     ,@(ucase--clauses-expand expr cases)))
    (_ `(b:then ,bindings ,@rest))))

(defmacro ucase (expr &rest clauses)
  "TODO: Documentation."
  (declare (indent 1))
  (cons 'ucond--bindings
        (ucase--clauses-expand expr clauses)))

(defun ucase--clauses-expand (expr clauses)
  (let* ((const (macroexp-const-p expr))
         (sym (if const expr (make-symbol "var")))
         (clauses-1 (if const clauses
                      (cons `(let* ((,sym ,expr))) clauses))))
    (cl-loop
     for clause in clauses-1
     collect (ucase--clause-desugar sym clause))))

(defun ucase--clause-desugar (exprsym clause)
  (pcase clause
    (`(let* . ,rest) (ucond--clause-let*-desugar rest))
    (`(ucase ,expr . ,rest)
     (ucond--clause-and-desugar
      '((_ t)) `(:and-ucase ,expr ,@rest)))
    (`(ucond . ,rest)
     (ucond--clause-and-desugar
      '((_ t)) `(:and-ucond ,@rest)))
    (`(,pattern . ,rest)
     (ucond--clause-and-desugar `((,pattern ,exprsym)) rest))
    (_ (error "Unknown clause"))))

(provide 'ucond)

;;; ucond.el ends here
