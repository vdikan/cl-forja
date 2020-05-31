(defpackage cl-forja
  (:use :cl)
  (:import-from #:serapeum
                #:assocdr)
  (:export :subst-accsyms
           :mk-calculation))

(in-package :cl-forja)


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
  "This macro creates a funcallable object (closure) representing a calculation.
A calculation is initialized with PLIST - an arbitrary property list. The rest of
the expressions in MK-CALCULATION body designate a RUNNER-FORM, that acts on
encapsulated plist of calculation parameters. It's copy is quoted and saved for
future provenance checks. Third piece of encapsulated data is the STATUS string
that is set to `new` upon creation.

Several names are reserved as data accessors to be used in the macro body. These are:
+ (ALL-PARAMS) : returns a copy of the whole PARAMS plist.
+ (GET-PARAM KEY) : returns value for key from this calculation's PARAMS plist;
identical to (GETF PARAMS KEY).
+ (SET-PARAM KEY) : sets value for key in calculation instance's PARAMS plist,
works only if such a key did not exist before.
+ (GET-STATUS) : returns a copy of the surrent status string.
+ (SET-STATUS STR) : sets STATUS to STR only if STATUS is `new`.
FIXME: implement error-handling instead of current permissive behavior (warn).

the behavior of the calculation object depends on keyword arguments it's called with:
+ no arguments (DEFAULT) : outputs calculation info in the form of:
(LIST `CL-Forja Calculation` STATUS PARAMS)
+ :GET :KEYWORD : outputs corresponding value from PARAMS plist; works on a copy of
PARAMS - therefore, not setf-able.
+ :ALL : outputs all the PARAMS plist. Internally calls ALL-PARAMS.
+ :STATUS : outputs current status string. Internally calls GET-STATUS.
+ :SHOW-RUNNER : outputs a copy of the RUNNER-FORM.
+ :RUN : Runs the calculation. Executes the compiled RUNNER-FORM (the macro body),
then sets its STATUS to `finished`. Consider composing other than `finished` STATUS
for exceptional conditions in the RUNNER-FORM."
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
                      (setf (getf ,g!params ,g!key) ,g!val)
                      (warn "Value for ~S is already set! Ignoring." ,g!key)))
                (,(assocdr 'get-status accsyms) () (copy-seq ,g!status))
                (,(assocdr 'set-status accsyms) (,g!str)
                  (if (string-equal ,g!status "new")
                      (setf ,g!status ,g!str)
                      (warn "Calculation status already set to ~S. Ignoring." ,g!status))))
         (lol:dlambda
          (:get (,g!kw) (getf (copy-list ,g!params) ,g!kw)) ; not setf-able into ,g!params
          (:all () (,(assocdr 'all-params accsyms)))
          (:status () (,(assocdr 'get-status accsyms)))
          (:show-runner () (copy-list ,g!runner-form))
          (:run ()
                (if (string-equal ,g!status "new")
                    (progn
                      ,@(subst-accsyms accsyms body)
                      (,(assocdr 'set-status accsyms) "finished"))
                    (warn "Calculation execution already resulted in ~S. Ignoring." ,g!status)))
          (t () (list "CL-Forja Calculation"
                      (,(assocdr 'get-status accsyms))
                      (,(assocdr 'all-params accsyms)))))))))
