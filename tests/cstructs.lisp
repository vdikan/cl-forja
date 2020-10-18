(defpackage cl-forja/tests/cstructs
  (:use :cl :rove))
(in-package :cl-forja/tests/cstructs)


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


(deftest test-cstruct-creation
  (testing "crystal structure instance should be created"
    (let ((cs (cl-forja/cstructs:make-cstruct
               :atoms '((Cl . #(#(0.0d0 0.0d0 0.0d0)))
                        (Na . #(#(0.5d0 0.5d0 0.5d0))))
               :kinds  (list
                        (cons 'Na (cl-forja/cstructs:make-chem-kind
                                   :number 11
                                   :mass 22.98977
                                   :siesta-pseudo #P"Na.psf"
                                   :qe-pseudo #P"Na.upf"))
                        (cons 'Cl (cl-forja/cstructs:make-chem-kind
                                   :number 17
                                   :mass 35.453
                                   :siesta-pseudo #P"Cl.psf"
                                   :qe-pseudo #P"Cl.upf")))
               :lattice (cl-forja/lattices:make-lattice-cF
                         :alat 5.6402 :units "Ang"))))
      (ok (typep (cl-forja/cstructs:cstruct-atoms cs)
                 'cl-forja/cstructs:atom-list))
      (ok (typep (cl-forja/cstructs:cstruct-kinds cs)
                 'cl-forja/cstructs:kind-reference))
      (ok (typep (cl-forja/cstructs:cstruct-lattice cs)
                 'cl-forja/lattices:lattice))
      (ok (typep (cl-forja/cstructs:cstruct-lattice cs)
                 'cl-forja/lattices:lattice-Cubic))
      (ok (typep (cl-forja/cstructs:cstruct-lattice cs)
                 'cl-forja/lattices:lattice-cF)))))


(deftest test-cstruct-counters
  (testing "counting atoms in cstruct"
    (let ((cs (cl-forja/cstructs:make-cstruct
               :atoms '((Mg . #(#(0.0000d0   0.0000d0  0.0000d0)
                                #(0.5000d0   0.5000d0  0.5000d0)))
                        (C  . #(#(0.2500d0   0.2500d0  0.2500d0)
                                #(-0.2500d0 -0.2500d0 -0.2500d0)))
                        (O  . #(#(0.5274d0  -0.0274d0  0.2500d0)
                                #(0.2500d0   0.5274d0 -0.0274d0)
                                #(0.0274d0   0.2500d0  0.5274d0)
                                #(0.5274d0   0.0274d0 -0.2500d0)
                                #(0.2500d0  -0.5274d0  0.0274d0)
                                #(0.0274d0  -0.2500d0 -0.5274d0))))
               :kinds (list
                       (cons 'C  (cl-forja/cstructs:make-chem-kind
                                  :number 6
                                  :mass 12.0107
                                  :qe-pseudo #P"C.pbe-n-kjpaw_psl.1.0.0.UPF"))
                       (cons 'O  (cl-forja/cstructs:make-chem-kind
                                  :number 8
                                  :mass 15.9994
                                  :qe-pseudo #P"O.pbe-n-kjpaw_psl.0.1.UPF"))
                       (cons 'Mg (cl-forja/cstructs:make-chem-kind
                                  :number 12
                                  :mass 24.305
                                  :qe-pseudo #P"Mg.pbe-n-kjpaw_psl.0.3.0.UPF"))
                       (cons 'Si (cl-forja/cstructs:make-chem-kind
                                  :number 14
                                  :mass 28.0855
                                  :qe-pseudo #P"Si.pbe-n-rrkjus_psl.1.0.0.UPF")))
               :lattice (cl-forja/lattices:make-lattice))))
      (ok (= (cl-forja/cstructs:number-of-atoms cs) 10))
      (ok (= (cl-forja/cstructs:number-of-kinds cs) 3)))))
