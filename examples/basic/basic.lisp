(cl:defpackage :cl-forja.examples.basic
  (:use :cl :cl-forja))

(cl:in-package :cl-forja.examples.basic)

(setf (symbol-function 'calc)
      (mk-calculation '(:x 42) (print "Hello")))

(cl:in-package :cl-forja.examples.basic)

(calc)

(cl:in-package :cl-forja.examples.basic)

(calc :show-runner)

(cl:in-package :cl-forja.examples.basic)

(calc :run)

(cl:in-package :cl-forja.examples.basic)

(calc)

(cl:in-package :cl-forja.examples.basic)

(calc :status)

(cl:in-package :cl-forja.examples.basic)

(calc :get :x)

(cl:in-package :cl-forja.examples.basic)

(setf (third (calc)) (list :y 84))

(cl:in-package :cl-forja.examples.basic)

(calc)

(cl:in-package :cl-forja.examples.basic)

(calc :all)

(cl:in-package :cl-forja.examples.basic)

(calc :run)

(cl:in-package :cl-forja.examples.basic)

(setf (symbol-function 'calc-copy)
      (eval `(mk-calculation (quote ,(calc :all))
               ,@(calc :show-runner))))

(cl:in-package :cl-forja.examples.basic)

(calc-copy)

(cl:in-package :cl-forja.examples.basic)

(calc-copy :run)

(cl:in-package :cl-forja.examples.basic)

(setf (symbol-function 'wrong-square-cube)
      (mk-calculation '(:x 42 :y 13)
        (set-param :x (* (get-param :x)
                         (get-param :x)))
        (set-param :y (* (get-param :y)
                         (get-param :y)
                         (get-param :y)))))

(cl:in-package :cl-forja.examples.basic)

(wrong-square-cube :run)
(wrong-square-cube)

(cl:in-package :cl-forja.examples.basic)

(setf (symbol-function 'square-cube)
      (mk-calculation '(:x 42 :y 13)
        (set-param :x3 (* (get-param :x)
                          (get-param :x)))
        (format t "Square of ~a is ~a~%" (get-param :x) (get-param :x3))
        (set-param :y3 (* (get-param :y)
                          (get-param :y)
                          (get-param :y)))
        (format t "Cube of ~a is ~a~%" (get-param :y) (get-param :y3))))

(cl:in-package :cl-forja.examples.basic)

(square-cube :run)
(square-cube)
