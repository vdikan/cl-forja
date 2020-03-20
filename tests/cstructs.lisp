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
      '#(#(1d0 2d0 3d0) #(4d0 5d0 6d0) #(7d0 8d0 9d0))))))
