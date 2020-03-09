(defpackage cl-forja
  (:use :cl :lisp-namespace)
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


(lol:defmacro! mk-calculation (plist &body body)
  `(let ((,g!params ,plist)            ; nslet? for/from lisp-namespace
         (,g!runner-form '(,@body))    ; TODO: print to string, no quoting
         (,g!status 'new))
     (labels ((all-params () ,g!params)
              (get-param (key) (getf ,g!params key))
              (set-param (key val) (setf (getf ,g!params key) val))
              (get-status () ,g!status)
              (set-status (sym) (setf ,g!status sym)))
       (lol:dlambda
        (:get (key) (get-param key))
        (:status () ,g!status)
        (:show-runner () ,g!runner-form)
        (:run () ,@body (set-status 'finished))
        (t () (cons (cons 'calculation ,g!status) (list ,g!params)))))))



;; ;;;;;;;test area;;;;;;;;
;; (defparameter props
;;   '(:label "si8"
;;     :alat  (10.16 10.26 10.36 10.46)
;;     :alat-units "Bohr"
;;     :code ("Siesta" "QE")
;;     :bands ("G" "X" "L" "K" "G")))

;; (print
;;  (macroexpand '(mk-calculation props
;;                (set-param :very-new '("Works?" "Yes!"))
;;                (set-param :rather-old '("Should work?" "Also true"))) ))


;; ;;;;;;;;;;;;;;;;;;;;
;; (define-namespace calcs)

;; (setf (symbol-calcs 'calctest)
;;       (mk-calculation props
;;         (set-param :very-new '("Works?" "Yes!"))
;;         (set-param :rather-old '("Should work?" "Also true"))))

;; (print
;;  (funcall (symbol-calcs 'calctest) :show-runner))

;; *calcs-table*

;; (loop
;;    for key being the hash-keys of *calcs-table*
;;    ;; using (hash-value value)
;;    do (print (funcall (symbol-calcs key) :show-runner)))
;;    ;; do (print (funcall value :show-runner)))
