(defpackage cl-forja/tests/main
  (:use :cl
        :cl-forja
        :rove))
(in-package :cl-forja/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-forja)' in your Lisp.

(deftest test-unroll-plist-f
  (testing "should unroll plists okay"
    (let* ((props '(:label "si8"
                    :alat  (10.16 10.26 10.36 10.46)
                    :alat-units "Bohr"
                    :code ("Siesta" "QE")
                    :bands ("G" "X" "L" "K" "G")))
           (unrolled
            (cl-forja:unroll-plist-f
             props :alat :code
             (list :bands #'(lambda (s) (format nil "~a-point" s))))))
      (ok (member '(:LABEL "si8"
                    :ALAT 10.16
                    :ALAT-UNITS "Bohr"
                    :CODE "Siesta"
                    :BANDS "L-point")
                  unrolled :test 'equal))
      (ok (member '(:LABEL "si8"
                    :ALAT 10.46
                    :ALAT-UNITS "Bohr"
                    :CODE "QE"
                    :BANDS "G-point")
                  unrolled :test 'equal)))))
