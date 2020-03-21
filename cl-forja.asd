(defsystem "cl-forja"
  :version "0.1.0"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("let-over-lambda"
               "serapeum"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "lattices")
                 (:file "cstructs")
                 (:file "templates"))
                :serial t))
  :description ""
  :in-order-to ((test-op (test-op "cl-forja/tests"))))

(defsystem "cl-forja/tests"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("cl-forja"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "cstructs"))))
  :description "Test system for cl-forja"
  :perform (test-op (op c) (symbol-call :rove :run c)))
