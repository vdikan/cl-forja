(defpackage cl-forja/tests/charts
  (:use :cl :rove))
(in-package :cl-forja/tests/charts)


;; NOTE: To run this test file, execute `(asdf:test-system :cl-forja)' in your Lisp.


(deftest test-quick-chart-data
    (testing "Should extraxt plot data from plist for quick chart (via `vgplot`)")
  (ok
   (equal
    (cl-forja/charts:quick-chart-data
     '((:x 5 :y 6 :z 8) (:x 10 :y 16 :z 18) (:x 15 :y 36 :z -28)) :x '((:y "Y-Data") :z))
    '((5 10 15) (6 16 36) "Y-Data" (5 10 15) (8 18 -28) "Z"))))
