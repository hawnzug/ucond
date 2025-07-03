;;; ucond-tests.el --- Tests for ucond.el  -*- lexical-binding: t -*-

;; Copyright (C) 2025  Zhuyang Wang

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ucond)

(ert-deftest ucond-tests-let* ()
  (should (equal (ucond (let* ((x 1))) (t x)) 1))
  (should (equal (ucond (let* ((x 1) (y (1+ x)))) (t (cons x y))) '(1 . 2)))
  (should (equal (ucond (let* ((x 1))) (let* ((y (1+ x)))) (t (cons x y)))
                 '(1 . 2)))
  (should (equal (ucond (let* ((`(,x ,y) (list 3 4)))) (t (list x y)))
                 '(3 4)))
  (should (equal (ucond
                  (let* ((x 1) (y 4)))
                  ((= x 3) (list x y))
                  (let* ((x 2)))
                  ((= x 3) (list x y))
                  (let* ((x 3)))
                  ((= x 3) (list x y)))
                 '(3 4)))
  ;; (should-error (ucond (let* ((x 1) (2 x)) :otherwise x)))
  (should (equal (ucond (let* ((1 2)) :otherwise 3)) 3))
  (should (equal (ucond (let* ((x 1) (2 x)) :otherwise 3) (t x)) 3)))
