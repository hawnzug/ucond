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
  (let* ((ft-current fall-through)
         (ft-next (or ft-current (make-symbol "fall-through"))))
    (if (null cases)
        `',ft-current
      (let ((rest (ucond--core-expand (cdr cases) ft-current)))
        (pcase (car cases)
          (`(c:then ,pattern ,expr . ,then)
           `(pcase ,expr
              (,pattern ,@then)
              (_ ,rest)))
          (`(c:else ,bindings . ,else)
           (let ((sym-body (make-symbol "body"))
                 (ex (ucond--core-else-expand bindings rest ft-current)))
             `(let ((,sym-body ,ex))
                (if (eq ',ft-current ,sym-body)
                    ,@(or else (list `',ft-current))
                  ,sym-body))))
          (`(c:cond ,pattern ,expr . ,nested-cases)
           (let* ((sym-body (make-symbol "body")))
             `(let ((,sym-body
                     (pcase ,expr
                       (,pattern ,(ucond--core-expand nested-cases ft-next)))))
                (if (eq ',ft-next ,sym-body) ,rest ,sym-body)))))))))

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
  "Nested cond with fall-through, pcase, and interleaving let.
The `ucond' construct is followed by a list of CLAUSES.
Return nil when CLAUSES is empty.
Each clause in CLAUSES can take one of the forms:

  (let* BINDINGS [:otherwise ELSE...])
  (match* BINDINGS BODY...)
  (match* BINDINGS :and-ucond UCOND-CLAUSES)
  (match* BINDINGS :and-ucase EXPR UCASE-CLAUSES)
  (when CONDITION [:otherwise ELSE...])
  (CONDITION)
  (CONDITION BODY...)
  (CONDITION :and-ucond UCOND-CLAUSES)
  (CONDITION :and-ucase EXPR UCASE-CLAUSES)
  (ucond UCOND-CLAUSES)
  (ucase EXPR UCASE-CLAUSES)

Only the let* and match* clauses are essential.
The rest clauses can be translated to let* or match* directly.

The (let* BINDINGS [:otherwise ELSE...]) clause
tries to make the BINDINGS available to the following clauses,
but it exits the `ucond' with ELSE if BINDINGS failed.
It has a list of BINDINGS and an optional ELSE branch
introduced by the :otherwise keyword.

Like `pcase-let*', each binding in BINDINGS has the form (PATTERN EXPR).
EXPR is evaluated and matched against PATTERN using `pcase'.
If EXPR matches PATTERN,
the variables introduced by PATTERN will be available
to the following BINDINGS.

If all the matchings succeed, all the variables introduced will be
available to the following CLAUSES, that is, the rest in `ucond'.
If any matching fails, exit the `ucond' by running ELSE...
and return the value of the last of the ELSE's.
When there is no :otherwise, or when ELSE... has zero expression,
it exits the current level of `ucond' and falls through if possible.
See the match* clause for details about fall-through.

The (match* BINDINGS BODY...) clause tries to make BINDINGS
and exits the `ucond' with BODY,
but if BINDINGS failed, it proceeds to the following clauses.
BINDINGS are the same as in the let* clause.

The (match* BINDINGS :and-ucond UCOND-CLAUSES) clause
tries to make BINDINGS, and if it succeeded,
insert the UCOND-CLAUSES into the `ucond' after this clause
with BINDINGS available locally,
and proceeds to the new following clauses.
Otherwise proceeds to the current following clauses directly.
In other words, if the BINDINGS succeeded,
starts a nested `ucond' with BINDINGS,
but will fall through to the outer clauses
if none of the inner clauses matches.
BINDINGS are the same as in the let* clause.
UCOND-CLAUSES are the same as the CLAUSES in `ucond'.

The (match* BINDINGS :and-ucase EXPR UCOND-CLAUSES) clause
works similarly to the previous clause,
but starts a nested `ucase' with fall-through.
See `ucase' for more details on EXPR and UCOND-CLAUSES.

The (when CONDITION [:otherwise ELSE...]) clause works
similarly to the let* clause,
except that BINDINGS are replaced by a single CONDITION expression.
It is equivalent to (let* (((guard CONDITION) t)) [:otherwise ELSE...]),
that is, a let* with only one binding checking if CONDITION is non-nil.

The (CONDITION ...) family works
similarly to the (match* BINDINGS ...) family.
It is equivalent to (match* (((guard CONDITION) t)) ...).
The (CONDITION BODY...) clause is exactly the same
as a normal clause in the traditional `cond',
except that the first expression in BODY cannot be
:and-ucase or :and-ucond.
The (CONDITION) clause is also the same as in `cond',
which returns CONDITION's value when it is non-nil.
To avoid any syntactic ambiguity,
CONDITION cannot be any of let*, match*, when, ucond, or ucase.
To work around, one can use (progn let*) in CONDITION, for example.

The (ucond UCOND-CLAUSES) and (ucase EXPR UCASE-CLAUSES) clauses
are equivalent to (t :and-ucond UCOND-CLAUSES)
and (t :and-ucase EXPR UCASE-CLAUSES) respectively."
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
  "Nested pcase with fall-through and interleaving let.
The `ucase' construct is followed by an EXPR to match,
and a list of CLAUSES.
Return nil when CLAUSES is empty.
Each clause in CLAUSES can take one of the forms:

  (let* BINDINGS [:otherwise ELSE...])
  (match* BINDINGS BODY...)
  (match* BINDINGS :and-ucond UCOND-CLAUSES)
  (match* BINDINGS :and-ucase EXPR UCASE-CLAUSES)
  (when CONDITION [:otherwise ELSE...])
  (PATTERN BODY...)
  (PATTERN :and-ucond UCOND-CLAUSES)
  (PATTERN :and-ucase EXPR-1 UCASE-CLAUSES)
  (ucond UCOND-CLAUSES)
  (ucase EXPR-1 UCASE-CLAUSES)

All clauses have the same meaning as in `ucond',
except for the three (PATTERN ...) clauses.
The pattern clauses are equivalent to the match* clauses
trying to match PATTERN with the value of EXPR,
that is, (match* ((PATTERN VAL-EXPR)) ...)."
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
