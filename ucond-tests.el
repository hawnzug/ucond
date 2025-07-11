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
  (should (equal (ucond (let* ((1 2)) :otherwise 3)) 3))
  (should (equal (ucond (let* ((x 1) (2 x)) :otherwise 3) (t x)) 3))
  (should (equal (ucond (let* ((x 1))) (let* ((2 x)) :otherwise x)) 1))
  (should-error (ucond (let* ((x 1) (2 x)) :otherwise x)))
  (should (equal (ucond
                  (let* ((x 1)))
                  (let* ((x 2) (3 4)) :otherwise x))
                 1))
  (should (equal (ucond
                  (let* ((x 1) (x 2)))
                  (let* ((x 3) (x 4)))
                  (t x))
                 4)))

(ert-deftest ucond-tests-match* ()
  (should-error (ucond (match* () t)))
  (should (equal (ucond (match* ((x 1)) x)) 1))
  (should (equal (ucond (match* ((x 1) (y (1+ x))) (+ x y))) 3))
  (should (equal (ucond (match* ((0 1)) 1)
                        (match* ((0 2)) 2)
                        (match* ((0 0)) 0))
                 0))
  (should (equal (ucond (match* ((x 1) (0 1)) x)) nil))
  (should-error (ucond (match* ((x 1) (0 1)) x)
                       (match* ((0 0)) x))))

(ert-deftest ucond-tests-match*-and-ucond ()
  (should (equal (ucond
                  (match* ((x 1))
                    :and-ucond
                    (match* ((y (1+ x))) (list x y))))
                 '(1 2)))
  (should (equal (ucond
                  (match* ((0 1)) :and-ucond (t 'no))
                  (t 'yes))
                 'yes))
  (should (equal (ucond
                  (match* ((x 1))
                    :and-ucond
                    (match* ((0 1)) 'a)
                    (match* ((0 2)) 'b)
                    (match* ((0 0)) 'c)
                    (match* ((0 3)) 'd)))
                 'c))
  (should (equal (ucond
                  (match* ((x 1))
                    :and-ucond
                    (match* ((0 1)) 'a)
                    (match* ((0 2)) 'b)
                    (match* ((0 3)) 'd))
                  (match* ((0 0)) 'c))
                 'c))
  (should (equal (ucond
                  (match* ((x 'yes))
                    :and-ucond
                    (match* ((0 0))
                      :and-ucond
                      (match* ((0 1)) 'a)
                      (match* ((0 2)) 'b))
                    (match* ((0 0))
                      :and-ucond
                      (match* ((0 x)) 'c)
                      (match* ((1 x)) 'd))
                    (match* ((0 0))
                      :and-ucond
                      (match* ((0 6)) 'e))
                    (match* (('yes x) (y x)) y)))
                 'yes))
  (should (equal (ucond
                  (match* ((x 0))
                    :and-ucond
                    (match* ((1 x)) 'no)
                    (let* ((x 1)))
                    (match* ((1 x)) 'yes)))
                 'yes))
  (should (equal (ucond
                  (match* ((0 0))
                    :and-ucond
                    (let* ((1 1))))
                  (match* ((2 2)) 'yes))
                 'yes))
  (should (equal (ucond
                  (match* ((0 0))
                    :and-ucond
                    (let* ((1 3))))
                  (match* ((2 2)) 'yes))
                 'yes))
  (should (equal (ucond
                  (match* ((0 0))
                    :and-ucond
                    (let* ((1 3)) :otherwise 'yes))
                  (match* ((2 2)) 'no))
                 'yes)))

(ert-deftest ucond-tests-when ()
  (should (equal (ucond
                  (when nil)
                  (t 'no))
                 nil))
  (should (equal (ucond
                  (when nil :otherwise 'yes)
                  (t 'no))
                 'yes))
  (should (equal (ucond
                  (ucond
                   (when nil)
                   (t (error "when")))
                  (t 'yes))
                 'yes)))

(ert-deftest ucond-tests-when-let* ()
  (should (equal (ucond (when-let* ((x nil)) :otherwise 'yes)) 'yes))
  (should (equal (ucond
                  (when-let* ((x 1) (y (1+ x))) :otherwise 'no)
                  (x y))
                 2))
  (should-error (ucond (when-let* ((x 1) (y nil)) :otherwise x))))

(ert-deftest ucond-tests-ucase ()
  (should (equal (ucond
                  (let* ((x 1)))
                  (ucase x
                    (2 'no)
                    (let* ((x 2)))
                    (2 'nono)
                    ((pred (= x)) 'x)
                    ((pred (= (1- x))) 'yes)))
                 'yes)))
