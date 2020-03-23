(defpackage cl-forja/siesta
  (:use :cl
        :cl-forja/templates
        :cl-forja/cstructs
        :cl-forja/lattices)
  (:import-from #:serapeum
                #:assocdr)
  (:export #:number-of-atoms-to-template
           #:number-of-kinds-to-template
           #:atom-list-to-template
           #:kinds-to-template
           #:lattice-to-template))

(in-package :cl-forja/siesta)


(defmethod number-of-atoms-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :siesta)))
  (plist-to-template
   (list :number-of-atoms (number-of-atoms cs)) tplt))


(defmethod number-of-kinds-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :siesta)))
  (plist-to-template
   (list :number-of-kinds (number-of-kinds cs)) tplt))


(defun siesta-atom-list (cs)
  (let ((res ""))
    (dotimes (i (length (cstruct-atoms cs)) res)
      (setf res
            (format nil "~a~&~a" res
                    (format nil"~{~&~{~,12@f ~,12@f ~,12@f ~d~}~}"
                            (map 'list (lambda (vec) (list (aref vec 0)
                                                           (aref vec 1)
                                                           (aref vec 2)
                                                           (+ i 1)))
                                 (cdr (nth i (cstruct-atoms cs))))))))))


(defmethod atom-list-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :siesta)))
  (plist-to-template
   (list :atom-list (siesta-atom-list cs)) tplt))


(defun siesta-kinds (cs)
  (let ((res ""))
    (dotimes (i (length (cstruct-atoms cs)) res)
      (let ((kind-sym (car (nth i (cstruct-atoms cs)))))
        (setf res
              (format nil "~a~&~a" res
                      (format nil "~{~&~4d ~4d  ~a~}"
                              (list
                               (+ i 1)
                               (chem-kind-number (assocdr kind-sym (cstruct-kinds cs)))
                               (string-capitalize (symbol-name kind-sym))))))))))


(defmethod kinds-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :siesta)))
  (plist-to-template
   (list :chem-kinds (siesta-kinds cs)) tplt))


(defgeneric siesta-lattice-properties (latt))

(defmethod siesta-lattice-properties ((latt lattice-Cubic))
  (list :alat-value (format nil "~,8f" (lattice-alat latt))
        :alat-units (format nil "~a" (lattice-units latt))
        :lattice-parameters (format nil "~&  1.0  1.0  1.0  90  90  90")))


(defmethod lattice-to-template
    ((tplt string) (cs cstruct) (code-spec (eql :siesta)))
  (plist-to-template
   (siesta-lattice-properties (cstruct-lattice cs)) tplt))