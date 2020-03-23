(defpackage cl-forja/tests/templates
  (:use :cl :rove
        :cl-forja/siesta))
(in-package :cl-forja/tests/templates)

(named-readtables:in-readtable lol:lol-syntax)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-forja)' in your Lisp.


(deftest test-template
  (testing "template substitution from plist"
    (let ((pl '(:alat 10.26
                :label "si"
                :system-name "Silicon"))
          (tplt
           #"SystemLabel ##:label:##
SystemName ##:system-name:##
LatticeConstant ##:alat:##"#))
      (ok (string-equal
           (cl-forja/templates:plist-to-template pl tplt)
           #"SystemLabel si
SystemName Silicon
LatticeConstant 10.26"#))))
  (testing "template substitution from smaller plist should partly fill"
    (let ((pl '(:alat 10.26
                :label "si"))
          (tplt
           #"SystemLabel ##:label:##
SystemName ##:system-name:##
LatticeConstant ##:alat:##"#))
      (ok (string-equal
           (cl-forja/templates:plist-to-template pl tplt)
           #"SystemLabel si
SystemName ##:system-name:##
LatticeConstant 10.26"#)))))
