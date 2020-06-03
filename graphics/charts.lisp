;;; Quick gnuplot charting utilities using vgplot

(defpackage cl-forja/charts
  (:use :cl :vgplot
        :cl-forja)
  ;; :cl-arrows)
  ;; (:import-from #:serapeum #:assocdr))
  (:export
   :quick-chart-data
   :quick-chart-plist
   :quick-chart))

(in-package :cl-forja/charts)


(defun quick-chart-data (plists-col x-kw y-data-specs)
  (let* ((xdat (mapcar (lambda (l) (getf l x-kw)) plists-col))
         (data nil))
    (loop for kwp in y-data-specs
          do (progn
               (push xdat data)
               (cond
                 ((keywordp kwp)
                  (progn
                    (push (mapcar (lambda (l) (getf l kwp)) plists-col) data)
                    (push (symbol-name kwp) data)))
                 ((listp kwp)
                  (destructuring-bind (kw desc-str &optional (func #'identity)) kwp
                    (push (mapcar (lambda (l) (funcall func (getf l kw))) plists-col) data)
                    (push desc-str data))))))
    (reverse data)))


(defun quick-chart-plist (plists-col x-kw y-data-specs
                          &key xlabel ylabel title)
  (vgplot:title (or title ""))
  (vgplot:xlabel (or xlabel ""))
  (vgplot:ylabel (or ylabel ""))
  (apply #'vgplot:plot (quick-chart-data plists-col x-kw y-data-specs)))


(defun quick-chart (session-lst x-kw y-data-specs &key xlabel ylabel title)
  (let* ((kws-lst (loop for kwp in (append (list x-kw) y-data-specs)
                        collect (if (keywordp kwp) kwp (car kwp))))
         (plist-col (loop for calc in session-lst
                          collect (apply #'get-params-from calc kws-lst))))
    (vgplot:title (or title ""))
    (vgplot:xlabel (or xlabel ""))
    (vgplot:ylabel (or ylabel ""))
    (apply #'vgplot:plot (quick-chart-data plist-col x-kw y-data-specs))))
