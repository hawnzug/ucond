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

(eval-when-compile (require 'cl-macs))

;;;; Core

(defmacro ucond--core (&rest cases)
  "The core language of `ucond'.
CASES is a list of cases. Each case can take one of the forms:

1. (c:then PATTERN EXPR THEN-BODY...).
2. (c:else BINDINGS ELSE-BODY...).
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

In the c:else form, the BINDINGS is a list of bindings of the form
(PATTREN EXPR).  It runs the pattern matchings sequentially, like a list
of c:then forms.  The difference is when all the pattern matchings
succeeded, it proceeds to the rest CASES with these bindings available,
and when any of the pattern matchings failed, it exits the `ucond--core'
with ELSE.

In the c:cond form, if PATTERN matches EXPR,
start a nested `ucond--core' on NESTED-CASES at the inner level.
The NESTED-CASES have the same forms of CASES.
The execution of NESTED-CASES is also the same,
except when all NESTED-CASES failed to match,
the control will return to the outer level,
that is, check the next CASE after this c:cond form.
In other words, this is a nested match
which falls through to the outer match when no inner CASE matches.
Note that the variables introduced by c:else in NESTED-CASES
are no long vaild after the fall-through to the outer level."
  (declare (indent nil))
  (ucond--core-expand cases nil))

(defun ucond--core-expand (cases fall-through)
  (pcase cases
    ('nil `',fall-through)
    (`((c:then ,pattern ,expr . ,then) . ,cases)
     (pcase-let ((`(,clauses . ,cases)
                  (ucond--core-thens-expand expr cases)))
       `(pcase ,expr
          (,pattern ,@then)
          ,@clauses
          (_ ,(ucond--core-expand cases fall-through)))))
    (`((c:else ,bindings . ,else) . ,cases)
     (let ((sym-body (make-symbol "body"))
           (rest (ucond--core-expand cases fall-through)))
       `(let ((,sym-body
               ,(ucond--core-else-expand bindings rest fall-through)))
          (if (eq ',fall-through ,sym-body)
              ,@(or else (list `',fall-through))
            ,sym-body))))
    (`((c:cond ,pattern ,expr . ,nested-cases) . ,cases)
     (let ((rest (ucond--core-expand cases fall-through))
           (sym-body (make-symbol "body"))
           (ft-next (or fall-through
                        (make-symbol "fall-through"))))
       `(let ((,sym-body
               (pcase ,expr
                 (,pattern ,(ucond--core-expand nested-cases ft-next))
                 (_ ',ft-next))))
          (if (eq ',ft-next ,sym-body) ,rest ,sym-body))))))

(defun ucond--core-thens-expand (expr cases)
  (let ((loop t) (clauses nil))
    (while loop
      (pcase cases
        ((and `((c:then ,pattern ,expr-1 . ,then) . ,rest-cases)
              (guard (eq expr expr-1))) ; TODO: equal?
         (push `(,pattern ,@then) clauses)
         (setq cases rest-cases))
        (_ (setq loop nil))))
    (cons (nreverse clauses) cases)))

(defun ucond--core-else-expand (bindings rest sym-else)
  (pcase bindings
    (`((,pattern ,expr))
     `(pcase ,expr
        (,pattern ,rest)
        ,@(when sym-else `((_ ',sym-else)))))
    (`((,pattern ,expr) . ,rest-bindings)
     `(pcase ,expr
        (,pattern ,(ucond--core-else-expand rest-bindings rest sym-else))
        ,@(when sym-else `((_ ',sym-else)))))
    (_ (error "Unknown binding"))))

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
         (cons `(c:else ,bindings ,@else) rest))))))

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
  "Nested and interleaved cond, pcase, and let* with else.
A `ucond' construct consists of a list of CLAUSES.  Return nil when
CLAUSES is empty.  Each clause in CLAUSES can take one of the forms:

  (let* BINDINGS [:otherwise ELSE...])
  (match* BINDINGS BODY...)
  (match* BINDINGS :and-ucond UCOND-CLAUSES)
  (match* BINDINGS :and-ucase EXPR UCASE-CLAUSES)
  (when-let* VARLIST [:otherwise ELSE...])
  (when CONDITION [:otherwise ELSE...])
  (CONDITION)
  (CONDITION BODY...)
  (CONDITION :and-ucond UCOND-CLAUSES)
  (CONDITION :and-ucase EXPR UCASE-CLAUSES)
  (ucond UCOND-CLAUSES)
  (ucase EXPR UCASE-CLAUSES)

where brackets mean optional.  Only the let* and match* clauses are
essential.  The rest clauses can be translated to let* or match*
directly.

A `ucond' construct can have nested ucond-like constructs, introduced by
the `ucond' clause, or by the :and-ucond keyword, for example, in the
match* clause.  In this documentation, these nested constructs will also
be called (inner) `ucond'.  And when we explain the meaning of a clause,
the clause might belong to the top-level `ucond' or a nested `ucond'.

The (let* BINDINGS [:otherwise ELSE...]) clause tries to make the
BINDINGS available to the clauses and nested sub-clauses after this let*
clause in the current `ucond'.  If BINDINGS failed, it exits the outmost
`ucond' with ELSE, or it falls through to the outer `ucond' when there
is no ELSE.

The let* clause consists of a list of BINDINGS and an optional ELSE
branch introduced by the :otherwise keyword.  Each binding in BINDINGS
has the form (PATTERN EXPR), just like in `pcase-let*'.  EXPR is
evaluated and matched against PATTERN using `pcase'.  If EXPR matches
PATTERN, the variables introduced by PATTERN will be available to the
following BINDINGS.

If all the matchings succeeded, all the variables introduced will be
available to the following clauses and nested sub-clauses in the current
`ucond'.  If any matching failed, exit the outmost `ucond' with ELSE.
However, when there is no :otherwise, or when ELSE is empty, it exits
the current level of `ucond' and falls through to the outer level.  If
there is no outer level, return nil.  See the match* :and-ucond clause
for details about fall-through.

The (match* BINDINGS BODY...) clause works like the let* clause, but
with the control flow flipped.  If BINDINGS succeeded, execute BODY with
all the variables introduced by BINDINGS available, and exit the outmost
`ucond' with the value of BODY.  If BINDINGS failed, proceed to the
following clauses.  BINDINGS are the same as in the let* clause.

The (match* BINDINGS :and-ucond UCOND-CLAUSES) clause works similarly to
a nested `ucond' in a match*: (match* BINDINGS (ucond UCOND-CLAUSES)).
The difference is that, if BINDINGS succeeded,
the (match* BINDINGS (ucond UCOND-CLAUSES)) clause will always exit
the outmost `ucond' with the value of the inner `ucond',
but (match* BINDINGS :and-ucond UCOND-CLAUSES) might fall through from
the inner `ucond' and proceed to the following clauses.  Fall-through
to the outer level can happen in one of these cases:
1. All clauses have been tried, but none exits and returns a value.
2. A let* or when clause failed, but it has no ELSE branch to return.

The (match* BINDINGS :and-ucase EXPR UCASE-CLAUSES) clause works
similarly to the previous :and-ucond clause, but starts a nested `ucase'
with fall-through.  See `ucase' for more details on EXPR and
UCASE-CLAUSES.

The (when-let* VARLIST [:otherwise ELSE...]) clause works similarly to
the let* clause, except that each element of VARLIST is a list (SYMBOL
VALUEFORM), same as the built-in `when-let*'.  It is equivalent
to (let* (((and VAR (guard VAR)) EXPR)...) [:otherwise ELSE...]).

The (when CONDITION [:otherwise ELSE...]) clause works similarly to the
let* clause, except that BINDINGS are replaced by a single CONDITION.
It is equivalent to (let* (((guard CONDITION) t)) [:otherwise ELSE...]),
or it is equivalent to (when-let* ((_ CONDITION)) [:otherwise ELSE...]).

The (CONDITION ...) family works similarly to the (match* BINDINGS ...)
family.  It is equivalent to (match* (((guard CONDITION) t)) ...).
The (CONDITION BODY...) clause is exactly the same as a normal clause in
the traditional `cond', except that the first expression in BODY cannot
be :and-ucase or :and-ucond.  The (CONDITION) clause is also the same as
in `cond', which returns CONDITION's value when it is non-nil.  To avoid
any syntactic ambiguity, CONDITION cannot be any of let*, match*, when,
ucond, or ucase.  To work around, one can use (progn let*) as CONDITION,
for example.

The (ucond UCOND-CLAUSES) and (ucase EXPR UCASE-CLAUSES) clauses are
equivalent to (t :and-ucond UCOND-CLAUSES) and (t :and-ucase EXPR
UCASE-CLAUSES) respectively.  See `ucase' for more details on EXPR and
UCASE-CLAUSES."
  (cons 'ucond--bindings
        (ucond--clauses-expand clauses)))

(defun ucond--clauses-expand (clauses)
  (mapcar #'ucond--clause-desugar clauses))

(defun ucond--clause-desugar (clause)
  (or
   (ucond--common-desugar clause)
   (pcase clause
     (`(,condition)
      (let ((sym (make-symbol "var")))
        `(b:then (((and (pred (not null)) ,sym) ,condition)) ,sym)))
     (`(,condition . ,rest)
      `(b:then (((guard ,condition) t)) ,@rest))
     (_ (error "Unknown clause")))))

(defun ucond--common-desugar (clause)
  "Desugar common CLAUSE in `ucond' and `ucase'."
  (pcase clause
    (`(let* ,bindings . ,rest)
     (ucond--clause-else-desugar bindings rest))
    (`(match* ,bindings . ,rest)
     (ucond--clause-and-desugar bindings rest))
    (`(when-let* ,bindings . ,rest)
     (ucond--clause-else-desugar
      (cl-loop for (var expr) in bindings
               collect `((and ,var (guard ,var)) ,expr))
      rest))
    (`(when ,condition . ,rest)
     (ucond--clause-else-desugar `(((guard ,condition) t)) rest))
    (`(ucase ,expr . ,rest)
     (ucond--clause-and-desugar '((_ t)) `(:and-ucase ,expr ,@rest)))
    (`(ucond . ,rest)
     (ucond--clause-and-desugar '((_ t)) `(:and-ucond ,@rest)))))

(defun ucond--clause-else-desugar (bindings rest)
  `(b:else ,bindings ,@(pcase rest
                         ('nil nil)
                         (`(:otherwise . ,else) else)
                         (_ (error "Missing :otherwise")))))

(defun ucond--clause-and-desugar (bindings rest)
  (pcase rest
    (`(:and-ucond . ,cases)
     `(b:cond ,bindings ,@(ucond--clauses-expand cases)))
    (`(:and-ucase ,expr . ,cases)
     `(b:cond ,bindings
                     ,@(ucase--clauses-expand expr cases)))
    (_ `(b:then ,bindings ,@rest))))

(defmacro ucase (expr &rest clauses)
  "Nested and interleaved pcase, cond, and let* with else.
The `ucase' construct consists of an EXPR to match, and a list of
CLAUSES.  Return nil when CLAUSES is empty.  Each clause in CLAUSES
can take one of the forms:

  (let* BINDINGS [:otherwise ELSE...])
  (match* BINDINGS BODY...)
  (match* BINDINGS :and-ucond UCOND-CLAUSES)
  (match* BINDINGS :and-ucase EXPR UCASE-CLAUSES)
  (when-let* VARLIST [:otherwise ELSE...])
  (when CONDITION [:otherwise ELSE...])
  (PATTERN BODY...)
  (PATTERN :and-ucond UCOND-CLAUSES)
  (PATTERN :and-ucase EXPR-1 UCASE-CLAUSES)
  (ucond UCOND-CLAUSES)
  (ucase EXPR-1 UCASE-CLAUSES)

All the clauses, except for the three (PATTERN ...) clauses, have the
same meaning as in `ucond'.  It is recommended to read the documentation
of `ucond' first.

As in `ucond', all the nested ucase-like constructs will also be called
`ucase' in this documentation.  They are introduced by the (ucase ...)
clause, and the :and-ucase keyword.  The fall-through in `ucase' works
the same as in `ucond'.

The (PATTERN ...) clauses are equivalent to the match* clauses with a
single binding (PATTERN VAL-EXPR), where VAL-EXPR is the value of EXPR,
and EXPR is the expression introduced by the closest `ucase'."
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
  (or
   (ucond--common-desugar clause)
   (pcase clause
     (`(,pattern . ,rest)
      (ucond--clause-and-desugar `((,pattern ,exprsym)) rest))
     (_ (error "Unknown clause")))))

(provide 'ucond)

;;; ucond.el ends here
