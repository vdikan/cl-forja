(defpackage cl-forja/tests/cstructs
  (:use :cl
        :cl-forja
        :rove))
(in-package :cl-forja/tests/cstructs)


;; NOTE: To run this test file, execute `(asdf:test-system :cl-forja)' in your Lisp.


(deftest test-coordinates-type
  (testing "should correctly define coordinates-arrays types"
    (ok
     (cl-forja/cstructs:coordinates-p
      (map 'vector (lambda (x) (float x 0.0d0)) '#(1d0 2 3d0))))
    (ok
     (cl-forja/cstructs:coordinates-array-p
      '#(#(1d0 2d0 3d0) #(4d0 5d0 6d0) #(7d0 8d0 9d0))))
    (ok
     (typep
      '#(#(1d0 2d0 3d0) #(4d0 5d0 6d0) #(7d0 8d0 9d0))
      'cl-forja/cstructs:coordinates-array))))


(deftest test-atom-list-type
  (testing "type checking for atom-list"
    (let ((atomlist '((Mg . #(#(0.0000d0   0.0000d0  0.0000d0)
                              #(0.5000d0   0.5000d0  0.5000d0)))
                      (C  . #(#(0.2500d0   0.2500d0  0.2500d0)
                              #(-0.2500d0 -0.2500d0 -0.2500d0)))
                      (O  . #(#(0.5274d0  -0.0274d0  0.2500d0)
                              #(0.2500d0   0.5274d0 -0.0274d0)
                              #(0.0274d0   0.2500d0  0.5274d0)
                              #(0.5274d0   0.0274d0 -0.2500d0)
                              #(0.2500d0  -0.5274d0  0.0274d0)
                              #(0.0274d0  -0.2500d0 -0.5274d0))))))
      (ok (cl-forja/cstructs:atom-list-cons-p (car atomlist)))
      (ok (cl-forja/cstructs:atom-list-p atomlist))
      (ok (typep atomlist 'cl-forja/cstructs:atom-list)))))


(deftest test-kind-reference-type
  (testing "type checking for kind-reference"
    (let ((refcard (list
                    (cons 'Mg (cl-forja/cstructs:make-chem-kind
                               :number 12
                               :mass 24.305
                               :qe-pseudo #P"Mg.pbe-n-kjpaw_psl.0.3.0.UPF"))
                    (cons 'Si (cl-forja/cstructs:make-chem-kind
                               :number 14
                               :mass 28.0855
                               :siesta-pseudo #P"Si.psf")))))
      (ok (cl-forja/cstructs:kind-reference-cons-p (cadr refcard)))
      (ok (cl-forja/cstructs:kind-reference-p refcard))
      (ok (typep refcard 'cl-forja/cstructs:kind-reference)))))
