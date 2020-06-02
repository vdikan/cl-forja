(defsystem "cl-forja"
  :version "0.1.0"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("let-over-lambda"
               "serapeum"
               "cl-ppcre"
               "cl-arrows"
               "vgplot")
  :components ((:module "src"
                :components
                ((:file "lattices")
                 (:file "cstructs")
                 (:file "templates")
                 (:file "main"))
                :serial t)
               (:module "plugins"
                :depends-on ("src")
                :components
                ((:file "siesta")
                 (:file "qe")))
               (:module "graphics"
                :depends-on ("src")
                :components
                ((:file "charts"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-forja/tests"))))

(defsystem "cl-forja/tests"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("cl-forja"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "charts")
                 (:file "cstructs")
                 (:file "templates")
                 (:file "main"))))
  :description "Test system for cl-forja"
  :perform (test-op (op c) (symbol-call :rove :run c)))
