;;; TODO: hierarchy of lattices needs more work
(defpackage cl-forja/lattices
  (:use :cl)
  (:export :lattice
           :lattice-p
           :make-lattice
           :copy-lattice
           :lattice-alat
           :lattice-units
           :lattice-Cubic
           :make-lattice-Cubic
           :lattice-cP
           :make-lattice-cP
           :lattice-cI
           :make-lattice-cI
           :lattice-cF
           :make-lattice-cF))


(in-package :cl-forja/lattices)


(defun units-p (thing)
  (member thing '("Ang" "Bohr") :test #'string-equal))


(deftype units-type ()
  `(satisfies units-p))


(defstruct lattice
  (alat 0.0 :type real)
  (units "Bohr" :type units-type))


(defstruct (lattice-Cubic (:include lattice)))

(defstruct (lattice-cP (:include lattice-Cubic)))
(defstruct (lattice-cI (:include lattice-Cubic)))
(defstruct (lattice-cF (:include lattice-Cubic)))
