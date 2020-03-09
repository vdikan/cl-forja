(defpackage cl-forja/tests/main
  (:use :cl
        :cl-forja
        :rove))
(in-package :cl-forja/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-forja)' in your Lisp.


(deftest test-unroll-plist-f
  (testing "should unroll plists okay"
    (let* ((props '(:label "si"
                    :alat  (10.16 10.26 10.36 10.46)
                    :alat-units "Bohr"
                    :bands ("G" "X" "L" "K" "G")))
           (unrolled
            (cl-forja:unroll-plist-f
             props :alat (list :bands
                               #'(lambda (s)
                                   (format nil "~a-point" s))))))
      (ok (member '(:LABEL "si"
                    :ALAT 10.16
                    :ALAT-UNITS "Bohr"
                    :BANDS "L-point")
                  unrolled :test 'equal))
      (ok (member '(:LABEL "si"
                    :ALAT 10.46
                    :ALAT-UNITS "Bohr"
                    :BANDS "G-point")
                  unrolled :test 'equal)))))


(deftest test-calculation-closure
  (testing "`mk-calculation` should return a dlambda-closure
 around loggable-style plist"
    (let* ((props '(:label "si"
                    :alat  10.26
                    :alat-units "Bohr"))
           (calc (cl-forja:mk-calculation props
                   (set-param :title "Si structure DFT calc")
                   (set-param :code  '("Siesta" "QE")))))
      ;; (setf (symbol-function 'calc)
      ;;       (cl-forja:mk-calculation props
      ;;         (set-param :title "Si structure DFT calc")
      ;;         (set-param :code  '("Siesta" "QE"))))
      (ok (= (funcall (symbol-function 'calc) :get :alat) 10.26))
      (ok (eq (funcall (symbol-function 'calc) :run) 'finished))
      )))
