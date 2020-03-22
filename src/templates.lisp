(defpackage cl-forja/templates
  (:use :cl)
  (:import-from #:cl-ppcre
                #:regex-replace
                #:all-matches-as-strings)
  (:export :plist-to-template))

(in-package :cl-forja/templates)


;; (defun keys-from-template (tplt)
;;   (all-matches-as-strings "(?<=##:)[-\\w]+(?=:##)" tplt))


(defun make-keyword (name)
  (values (intern (string-upcase name) "KEYWORD")))


(defun plist-to-template (pl tplt)
  (let ((tplts (copy-seq tplt))
        (mtch (all-matches-as-strings "(?<=##:)[-\\w]+(?=:##)" tplt)))
    (loop
       for kws in (mapcar (lambda (kw) (format nil "##:~a:##" kw)) mtch)
       and rs in (mapcar (lambda (kw) (format nil "~a" (getf pl (make-keyword kw)))) mtch)
       do (setf tplts (regex-replace kws tplts rs)))
    tplts))
