;;; -*- coding: utf-8 -*-
;;;
;;; dynamic.el -- test dynamic scope

(setq x 10)

(defun one ()
  x)

(defun two ()
  (let ((x 5))
	(one)))

(insert (format "%d" (two)))
