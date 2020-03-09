(defpackage cl-forja
  (:use :cl)
  (:export :unroll-plist-f))
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
