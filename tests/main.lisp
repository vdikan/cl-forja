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


(deftest test-subst-accsyms
  (testing "`subst-accsyms` should substitute syms through tree form"
    (ok
     (equal
      (cl-forja:subst-accsyms
       '((one . "one") (two . "two") (three . "three"))
       '(5 two (3 4 (7 one)) (2 one 8) 1 one 9 (three 2)))
      '(5 "two" (3 4 (7 "one")) (2 "one" 8) 1 "one" 9 ("three" 2))))))


(deftest test-calculation-closure
  (testing "`mk-calculation` should return a dlambda-closure around plist"
    (let* ((props '(:label "si"
                    :alat  10.26
                    :alat-units "Bohr"))
           (calc (cl-forja:mk-calculation props
                   (set-param :title "Si structure DFT calc")
                   (set-param :code  '("Siesta" "QE")))))
      ; Test prepared calculation:
      (ok (equal (funcall calc)
                 (list "CL-Forja Calculation" "new" props)))
      (ok (equal (funcall calc :get :alat) 10.26))
      (ng (equal (funcall calc :get :code) '("Siesta" "QE")))
      (ok (string-equal (funcall calc :status) "new"))
      ; Then run it:
      (ok (eq (funcall calc :run) "finished"))
      (ok (equal (funcall calc :get :code) '("Siesta" "QE")))
      (ok (string-equal (funcall calc :status) "finished"))
      (ok (equal (funcall calc :show-runner)
                 '((set-param :title "Si structure DFT calc")
                   (set-param :code  '("Siesta" "QE"))))))))


(deftest test-get-should-work-on-a-copy
  (testing "calculations` :get should work on a PARAMS plist copy (persistence)"
    (let* ((calc (cl-forja:mk-calculation '(:foo "bar")))
           (plc (funcall calc :get :foo)))
      (setf plc "BaZ")
      (ok (string-equal (funcall calc :get :foo) "bar")))))


(deftest test-set-param
  (testing "set-param should set only not yet existing properties (persistence)"
    (let ((calc (cl-forja:mk-calculation '(:foo "bar")
                  (set-param :fee "bee")
                  (set-param :foo "BaZ"))))
      (ok (string-equal (funcall calc :run) "finished"))
      (ok (string-equal (funcall calc :get :fee) "bee"))
      (ok (string-equal (funcall calc :get :foo) "bar")))))


(deftest test-set-status
  (testing "set-status should only set status from `new` (persistence)."
    (let ((calc (cl-forja:mk-calculation '(:foo "bar")
                  (set-status "not-new"))))
      (ng (string-equal (funcall calc :run) "finished"))
      (ok (string-equal (funcall calc :status) "not-new")))))
