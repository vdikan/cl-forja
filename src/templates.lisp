(defpackage cl-forja/templates
  (:use :cl) ; :cl-arrows)
  (:import-from #:cl-ppcre
                #:regex-replace
                #:all-matches-as-strings)
  (:export :keys-from-template
           :plist-to-template))
;           :cstruct-to-template))

(in-package :cl-forja/templates)


(defun keys-from-template (tplt)
  (all-matches-as-strings "(?<=##:)[-\\w]+(?=:##)" tplt))


(defun make-keyword (name)
  (values (intern (string-upcase name) "KEYWORD")))


(defun plist-to-template (pl tplt)
  "TODO: refactor & descr"
  (let ((tplts (copy-seq tplt))
        (mtch (keys-from-template tplt)))
    (loop
       for kws in (mapcar (lambda (kw) (format nil "##:~a:##" kw)) mtch)
       and rs in (mapcar (lambda (kw) (format nil "~a" (getf pl (make-keyword kw)))) mtch)
       do (if (not (string-equal rs "NIL"))
              (setf tplts (regex-replace kws tplts rs))))
    tplts))


(defgeneric number-of-atoms-to-template (tplt cstruct code-spec))

(defgeneric number-of-kinds-to-template (tplt cstruct code-spec))

(defgeneric kinds-to-template (tplt cstruct code-spec))

(defgeneric lattice-to-template (tplt cstruct code-spec))

(defgeneric atom-list-to-template (tplt cstruct code-spec))

(defgeneric cstruct-to-template (tplt cstruct code-spec))

;; (defun cstruct-to-template (init-tplt cstruct code-spec)
;;   "Thread INIT-TPLT string through the methods that populate
;; corresponding tag placeholders with data from CSTRUCT in a plugin
;; format selected by CODE-SPEC keyword."
;;   (-> init-tplt
;;       (number-of-atoms-to-template cstruct code-spec)
;;       (number-of-kinds-to-template cstruct code-spec)
;;       (kinds-to-template cstruct code-spec)
;;       (lattice-to-template cstruct code-spec)
;;       (atom-list-to-template cstruct code-spec)))
