;;; -*- coding: utf-8; lexical-binding: t -*-
;;;                    ^^^^^^^^^^^^^^^^^^
;;; lexical.el -- test lexical scope

(setq x 10)

(defun one ()
  x)

(defun two ()
  (let ((x 5))
	(one)))

(insert (format "%d" (two)))

dd