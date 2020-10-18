(defpackage cl-forja/tests/borrows
  (:use :cl :rove
        :cl-forja/borrows))
(in-package :cl-forja/tests/borrows)


(deftest test-utils
  (testing "assocdr should work"
    (ok (eq 'd (assocdr 'c '((a . b) (c . d))))))
  (testing "flatten should work"
    (ok (equal '(1 2 3 4 5 6 7 8 9 10)
               (flatten '(1 2 3 (4 5 (6 7) 8 9) (10)))))))


(deftest test-dlambda
  (testing "dlambda macro"
    (let ((dl (dlambda
               (:something-special ()
                                   (format nil "SPECIAL"))
               (t (&rest args)
                  (format nil "DEFAULT: ~a" args)))))
      (ok (string-equal "DEFAULT: NIL" (funcall dl)))
      (ok (string-equal "DEFAULT: (1 2 3)" (funcall dl 1 2 3)))
      (ok (string-equal "SPECIAL" (funcall dl :something-special))))))
