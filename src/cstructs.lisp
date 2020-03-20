(defpackage cl-forja/cstructs
  (:use :cl)
  (:export :coordinates-p
           :coordinates-array-p
           :coordinates-array))

(in-package :cl-forja/cstructs)


(defstruct chem-element
  (number 0 :type integer)
  (mass 0.0 :type short-float)
  (siesta-pseudo (make-pathname :name nil) :type pathname)
  (qe-pseudo (make-pathname :name nil) :type pathname))

;; To tests
;; (null (pathname-name (make-pathname :name nil)))

(defun coordinates-p (thing)
  (and (arrayp thing)
       (= (length thing) 3)
       (every
        (lambda (x) (typep x 'double-float))
        thing)))


(defun coordinates-array-p (thing)
  (and (arrayp thing)
       (every #'coordinates-p thing)))


(deftype coordinates-array ()
  `(satisfies coordinates-array-p))
