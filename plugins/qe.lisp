(defpackage cl-forja/qe
  (:use :cl
        :cl-arrows
        :cl-forja/templates
        :cl-forja/cstructs
        :cl-forja/lattices)
  (:import-from #:serapeum
                #:assocdr))

(in-package :cl-forja/qe)


(defmethod cl-forja/templates:number-of-atoms-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :qe)))
  (plist-to-template
   (list :number-of-atoms (number-of-atoms cs)) tplt))


(defmethod cl-forja/templates:number-of-kinds-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :qe)))
  (plist-to-template
   (list :number-of-kinds (number-of-kinds cs)) tplt))


(defun qe-atom-list (cs)
  (let ((res ""))
    (dolist (l (cstruct-atoms cs) res)
      (setf res
            (format nil "~a~&~a" res
                    (format nil"~{~&~{~4a ~,12@f ~,12@f ~,12@f~}~}"
                            (map 'list (lambda (vec) (list
                                                      (string-capitalize
                                                       (symbol-name (car l)))
                                                      (aref vec 0)
                                                      (aref vec 1)
                                                      (aref vec 2)))
                                 (cdr l))))))))


(defmethod cl-forja/templates:atom-list-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :qe)))
  (plist-to-template
   (list :atom-list (qe-atom-list cs)) tplt))


(defun qe-kinds (cs)
  (let ((res ""))
    (dolist (l (cstruct-atoms cs) res)
      (let ((kind-sym (car l)))
        (setf res
              (format nil "~a~&~a" res
                      (format nil "~{~&~4a ~f ~16@a~}"
                              (list
                               (string-capitalize (symbol-name kind-sym))
                               (chem-kind-mass (assocdr kind-sym (cstruct-kinds cs)))
                               (chem-kind-qe-pseudo (assocdr kind-sym (cstruct-kinds cs)))))))))))


(defmethod cl-forja/templates:kinds-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :qe)))
  (plist-to-template
   (list :chem-kinds (qe-kinds cs)) tplt))


;;; On-lattice-dispatch section.
;; Well, have a look at this:
;; https://www.quantum-espresso.org/Doc/INPUT_PW.html#ibrav

(declaim (ftype (function (lattice) real) celldm1))

(defun celldm1 (latt)
  "Get the celldm1 for the LATTice in corect units, that is `Bohr`."
  (let ((alat (lattice-alat latt)))
    (if (string-equal (lattice-units latt) "Ang")
        (* alat 1.8897259885)           ; convert Ang to Bohr
        alat)))


(defgeneric qe-lattice-properties (latt))

(defmethod qe-lattice-properties ((latt lattice-Cubic))
  (list :ibrav "1" :celldm1 (format nil "~,8f" (celldm1 latt))))

(defmethod qe-lattice-properties ((latt lattice-cF))
  (list :ibrav "2" :celldm1 (format nil "~,8f" (celldm1 latt))))


(defmethod cl-forja/templates:lattice-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :qe)))
  (plist-to-template
   (qe-lattice-properties (cstruct-lattice cs)) tplt))
