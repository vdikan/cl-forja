(defpackage cl-forja
  (:use :cl :serapeum)
  (:export :unroll-plist-f
           :subst-accsyms
           :mk-calculation))

(in-package :cl-forja)


(defun unroll-plist-f (plist &rest key-conses)
  "An utility function that creates plist-collections from lists
in parent plist. Arguments accepted are keywords or conses (KW FUNC)
where FUNC is applied to superseeding list member upon creation of plist."
  (let* ((key-cons (car key-conses))
         (key  (if key-cons
                   (if (consp key-cons)
                       (car key-cons) key-cons)))
         (vals (getf plist key)))
    (if (null key)
        (list plist)
        (let ((unrolled nil)
              (unroll-func (if (consp key-cons)
                               (if (consp (cdr key-cons))
                                   (or (cadr key-cons) #'identity)
                                   (or (cdr key-cons) #'identity))
                               #'identity)))
          (loop
             for val in vals
             do (progn
                  (let ((new-list (copy-list plist)))
                    (setf (getf new-list key)
                          (funcall unroll-func val))
                    (setf unrolled
                          (nconc unrolled (list new-list))))))
          (let ((recur nil))
            (dolist (r unrolled recur)
              (setf recur
                    (append recur (apply #'unroll-plist-f r (cdr key-conses)))))
            recur)))))


(defun symnames-equalp (sym1 sym2)
  "Compare names if SYM1 and SYM2 are symbols."
  (if (and (symbolp sym1) (symbolp sym2))
      (string-equal (symbol-name sym1) (symbol-name sym2))))


(defun subst-accsyms (accsyms form-tree)
  "Substitutes similarly named symbols in FORM-TREE with assoc values from ACCSYMS."
  (let ((result-tree form-tree))
    (dolist (asc accsyms result-tree)
      (setf result-tree
            (subst (cdr asc) (car asc) result-tree :test 'symnames-equalp)))))


(lol:defmacro! mk-calculation (plist &body body)
  "TODO: edit the description along with persistency tests."
  (let* ((accessors-list '(all-params get-param set-param get-status set-status))
         (accsyms (mapcar (lambda (acc) `(,acc . ,(gensym (symbol-name acc))))
                          accessors-list)))
    `(let ((,g!params ,plist)            ; nslet? for/from lisp-namespace
           (,g!runner-form '(,@body))    ; TODO: print to string, no quoting, no package name saving(?)
           (,g!status "new"))
       (labels ((,(assocdr 'all-params accsyms) () (copy-list ,g!params))
                (,(assocdr 'get-param  accsyms) (,g!key) (getf ,g!params ,g!key)) ; setf-able
                (,(assocdr 'set-param  accsyms) (,g!key ,g!val)
                  (if (not (getf ,g!params ,g!key))
                      (setf (getf ,g!params ,g!key) ,g!val)))
                (,(assocdr 'get-status accsyms) () (copy-seq ,g!status))
                (,(assocdr 'set-status accsyms) (,g!str) (setf ,g!status ,g!str)))
         (lol:dlambda
          (:get (,g!kw) (getf (copy-list ,g!params) ,g!kw)) ; not setf-able into ,g!params
          (:status () (,(assocdr 'get-status accsyms)))
          (:show-runner () (copy-list ,g!runner-form))
          (:run () ,@(subst-accsyms accsyms body)
                (,(assocdr 'set-status accsyms) "finished"))
          (t () (,(assocdr 'all-params accsyms)))))))) ; TODO: edit default output of this guy
