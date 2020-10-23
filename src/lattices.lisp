(defpackage cl-forja/lattices
  (:use :cl)
  (:export
   :lattice
   :lattice-p
   :lattice-alat
   :lattice-units
   :lattice-c/a
   :lattice-b/a
   :lattice-alpha
   :lattice-beta
   :lattice-gamma
   :make-lattice
   :make-lattice-cP
   :make-lattice-cI
   :make-lattice-cF
   :make-lattice-hP
   :make-lattice-hR
   :make-lattice-tP
   :make-lattice-tI
   :make-lattice-oP
   :make-lattice-oS
   :make-lattice-oF
   :make-lattice-oI
   :make-lattice-mP
   :make-lattice-mS
   :make-lattice-aP))

(in-package :cl-forja/lattices)


(defun units-p (thing)
  (member thing '("Ang" "Bohr") :test #'string-equal))


(deftype units-type ()
  `(satisfies units-p))


(defstruct lattice
  (alat 0.0 :type real)
  (units "Bohr" :type units-type))

(defun lattice-c/a (instance)
  (handler-case
      (slot-value instance 'c/a)
    (simple-error () 1.0)))

(defun lattice-b/a (instance)
  (handler-case
      (slot-value instance 'b/a)
    (simple-error () 1.0)))

(defun lattice-alpha (instance)
  (handler-case
      (slot-value instance 'alpha)
    (simple-error () 90.0)))

(defun lattice-beta (instance)
  (handler-case
      (slot-value instance 'beta)
    (simple-error () 90.0)))

(defun lattice-gamma (instance)
  (handler-case
      (slot-value instance 'gamma)
    (simple-error () 90.0)))


(defstruct (lattice-Cubic (:include lattice)))

(defstruct (lattice-cP (:include lattice-Cubic)))

(defstruct (lattice-cI (:include lattice-Cubic)))

(defstruct (lattice-cF (:include lattice-Cubic)))


(defstruct (lattice-Hexagonal (:include lattice)))

(defstruct (lattice-hP (:include lattice-Hexagonal))
  (c/a 1.0 :type real))

(defstruct (lattice-hR (:include lattice-Hexagonal))
  (gamma 90.0 :type real))


(defstruct (lattice-Tetragonal (:include lattice)))

(defstruct (lattice-tP (:include lattice-Tetragonal))
  (c/a 1.0 :type real))

(defstruct (lattice-tI (:include lattice-Tetragonal))
  (c/a 1.0 :type real))


(defstruct (lattice-Orthorhombic (:include lattice)))

(defstruct (lattice-oP (:include lattice-Orthorhombic))
  (b/a 1.0 :type real)
  (c/a 1.0 :type real))

(defstruct (lattice-oS (:include lattice-Orthorhombic))
  (b/a 1.0 :type real)
  (c/a 1.0 :type real))

(defstruct (lattice-oF (:include lattice-Orthorhombic))
  (b/a 1.0 :type real)
  (c/a 1.0 :type real))

(defstruct (lattice-oI (:include lattice-Orthorhombic))
  (b/a 1.0 :type real)
  (c/a 1.0 :type real))


(defstruct (lattice-Monoclinic (:include lattice)))

(defstruct (lattice-mP (:include lattice-Monoclinic))
  (b/a 1.0 :type real)
  (c/a 1.0 :type real)
  (beta 90.0 :type real))

(defstruct (lattice-mS (:include lattice-Monoclinic))
  (b/a 1.0 :type real)
  (c/a 1.0 :type real)
  (beta 90.0 :type real))


(defstruct (lattice-Triclinic (:include lattice)))

(defstruct (lattice-aP (:include lattice-Triclinic))
  (b/a 1.0 :type real)
  (c/a 1.0 :type real)
  (alpha 90.0 :type real)
  (beta  90.0 :type real)
  (gamma 90.0 :type real))
