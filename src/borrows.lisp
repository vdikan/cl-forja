;; Inspirations borrowed from:
;; :Alexandria - https://github.com/keithj/alexandria
;; :Serapeum - https://github.com/ruricolist/serapeum
;; :Let-over-Lambda - https://github.com/thephoeron/let-over-lambda
;;                  - https://letoverlambda.com/
(defpackage cl-forja/borrows
  (:use :cl)
  (:documentation "Fundamental macro-definitions from Let-over-Lambda
and utilities borrowed from projects like :alexandria and :serapeum.
Re-implemented here in order to minimize dependencies of the final system.
With credit and gratitude to the authors of original definitions.")
  (:export #:assocdr
           #:flatten
           #:defmacro!
           #:dlambda))
(in-package cl-forja/borrows)


(declaim (inline assocdr))
(defun assocdr (item alist &rest args &key &allow-other-keys)
  "Like (cdr (assoc ...)). Borrowed from :SERAPEUM"
  (let ((found (apply #'assoc item alist args)))
    (values (cdr found) found)))


;; Doug Hoyte's macros up to dlambda:
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))


  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))


  (defun flatten (x)
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                     ((atom x) (cons x acc))
                     (t (rec
                         (car x)
                         (rec (cdr x) acc))))))
      (rec x nil)))


  (defun parse-body (body &key documentation whole)
    "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given. Borrowed from :ALEXANDRIA"
    (let ((doc nil)
          (decls nil)
          (current nil))
      (tagbody
       :declarations
         (setf current (car body))
         (when (and documentation (stringp current) (cdr body))
           (if doc
               (error "Too many documentation strings in ~S." (or whole body))
               (setf doc (pop body)))
           (go :declarations))
         (when (and (listp current) (eql (first current) 'declare))
           (push (pop body) decls)
           (go :declarations)))
      (values body (nreverse decls) doc)))


  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))


  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2)))


  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2))))


(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body)))))


(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro/g! ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body))))))


(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                   (list (car d)))
              (apply (lambda ,@(cdr d))
                     ,(if (eq t (car d))
                          g!args
                          `(cdr ,g!args)))))
          ds))))
