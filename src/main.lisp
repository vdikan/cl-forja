(defpackage cl-forja
  (:use :cl :serapeum)
  (:export :unroll-plist-f
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


(defun subst-accsyms (accsyms form-tree)
  "Substitutes symbols in FORM-TREE with assoc values from ACCSYMS."
  (let ((result-tree form-tree))
    (dolist (asc accsyms result-tree)
      (setf result-tree
            (subst (cdr asc) (car asc) result-tree)))))


;; (lol:defmacro! mk-calculation (plist &body body)
;;   `(let ((,g!params ,plist)            ; nslet? for/from lisp-namespace
;;          (,g!runner-form '(,@body))    ; TODO: print to string, no quoting
;;          (,g!status 'new))
;;      (labels ((all-params () ,g!params)
;;               (,g!get-param (,g!key) (getf ,g!params ,g!key))
;;               (set-param (key val) (setf (getf ,g!params key) val))
;;               (get-status () ,g!status)
;;               (set-status (sym) (setf ,g!status sym)))
;;        (lol:dlambda
;;         (:get (key) (get-param key))
;;         (:status () ,g!status)
;;         (:show-runner () ,g!runner-form)
;;         (:run () (progn ,@body) (set-status 'finished))
;;         (t () (cons (cons 'calculation ,g!status) (list ,g!params)))))))


;; (lol:defmacro! draft (plist &body body)
;;   (let* ((accessors-list '(all-params get-param set-param get-status set-status))
;;          (accsyms (mapcar (lambda (acc) `(,acc . ,(gensym (symbol-name acc))))
;;                           accessors-list)))
;;     `(let ((,g!params ,plist)            ; nslet? for/from lisp-namespace
;;            (,g!runner-form '(,@body))    ; TODO: print to string, no quoting
;;            (,g!status 'new))
;;        (labels ((,(cdr (assoc 'all-params accsyms)) () ,g!params)
;;                 (,(cdr (assoc 'get-param  accsyms)) (,g!key) (getf ,g!params ,g!key))
;;                 (,(cdr (assoc 'set-status accsyms)) (,g!sym) (setf ,g!status ,g!sym)))
;;          (lol:dlambda
;;           (:get (,g!kw) (,(cdr (assoc 'get-param accsyms)) ,g!kw))
;;           (:status () ,g!status)
;;           (:show-runner () ,g!runner-form)
;;           (:run () ,@(subst-accsyms accsyms body)
;;                 (,(cdr (assoc 'set-status accsyms)) 'finished))
;;           (t () (cons (cons 'calculation ,g!status) (list ,g!params))))
;;          ))))
