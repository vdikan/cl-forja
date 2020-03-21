(defpackage cl-forja/cstructs
  (:use :cl)
  (:export :coordinates-p
           :coordinates-array-p
           :coordinates-array
           :atom-list-cons-p
           :atom-list-p
           :atom-list
           :chem-kind
           :chem-kind-p
           :make-chem-kind
           :copy-chem-kind
           :chem-kind-number
           :chem-kind-mass
           :chem-kind-siesta-pseudo
           :chem-kind-qe-pseudo
           :kind-reference-cons-p
           :kind-reference-p
           :kind-reference))

(in-package :cl-forja/cstructs)


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


(defun atom-list-cons-p (thing)
  (and (consp thing)
       (symbolp (car thing))
       (coordinates-array-p (cdr thing))))


(defun atom-list-p (thing)
  (and (listp thing)
       (every #'atom-list-cons-p thing)))


(deftype atom-list ()
  `(satisfies atom-list-p))


(defstruct chem-kind
  (number 0 :type integer)
  (mass 0.0 :type short-float)
  (siesta-pseudo (make-pathname :name nil) :type pathname)
  (qe-pseudo (make-pathname :name nil) :type pathname))


(defun kind-reference-cons-p (thing)
  (and (consp thing)
       (symbolp (car thing))
       (chem-kind-p (cdr thing))))


(defun kind-reference-p (thing)
  (and (listp thing)
       (every #'kind-reference-cons-p thing)))


(deftype kind-reference ()
  `(satisfies kind-reference-p))
