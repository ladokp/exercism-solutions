(defpackage #:raindrops
  (:use #:common-lisp)
  (:export #:convert))

(in-package #:raindrops)

(defun convert (n)
  (let
     ((result (format nil "~[Pling~]~[Plang~]~[Plong~]" (rem n 3) (rem n 5) (rem n 7))))
     (if (zerop (length result)) (write-to-string n) result))) 