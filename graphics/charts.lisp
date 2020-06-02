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


(defun quick-chart-data (plists-col x-kw kwpairs)
  (let* ((xdat (mapcar (lambda (l) (getf l x-kw)) plists-col))
         (data nil))
    (loop for kwp in kwpairs
          do (progn
               (push xdat data)
               (cond
                ((keywordp kwp)
                 (progn
                   (push (mapcar (lambda (l) (getf l kwp))
                                 plists-col) data)
                   (push (symbol-name kwp) data)))
                ((listp kwp)
                 (progn
                   (push (mapcar (lambda (l) (getf l (car kwp)))
                                 plists-col) data)
                   (push (cadr kwp) data))))))
    (reverse data)))


(defun quick-chart-plist (plists-col x-kw kwpairs
                          &key xlabel ylabel title)
  (vgplot:title (or title ""))
  (vgplot:xlabel (or xlabel ""))
  (vgplot:ylabel (or ylabel ""))
  (apply #'vgplot:plot (quick-chart-data plists-col x-kw kwpairs)))


(defun quick-chart (session-lst x-kw kwpairs &key xlabel ylabel title)
  (let* ((kws-lst (loop for kwp in (append (list x-kw) kwpairs)
                        collect (if (keywordp kwp) kwp (car kwp))))
         (plist-col (loop for calc in session-lst
                          collect (apply #'get-params-from calc kws-lst))))
    (vgplot:title (or title ""))
    (vgplot:xlabel (or xlabel ""))
    (vgplot:ylabel (or ylabel ""))
    (apply #'vgplot:plot (quick-chart-data plist-col x-kw kwpairs))))
