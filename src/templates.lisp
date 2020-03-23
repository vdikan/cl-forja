(defpackage cl-forja/templates
  (:use :cl)
  (:import-from #:cl-ppcre
                #:regex-replace
                #:all-matches-as-strings)
  (:export :plist-to-template))

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
