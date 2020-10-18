(defsystem "cl-forja"
  :version "0.1.0"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("let-over-lambda"
               "serapeum"
               "cl-ppcre"
               "cl-arrows")
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
                 (:file "qe"))))
  :description "Side-effects tracking system for rapid computational workflows prototyping."
  :in-order-to ((test-op (test-op "cl-forja/tests"))))


(defsystem "cl-forja/tests"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("cl-forja"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "cstructs")
                 (:file "templates")
                 (:file "main"))))
  :description "Test system for cl-forja"
  :perform (test-op (op c) (symbol-call :rove :run c)))


;; NOTE: To run this test file, execute `(asdf:test-system :cl-forja)' in your Lisp.
