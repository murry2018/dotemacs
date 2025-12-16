;;; pref-lib --- functions/macros library
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables
;;
(defconst *use-ivy* nil)
(defconst *use-helm* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library funcs/macros
;;

(defun pref/remove-from-tree (elem tree test)
  "Recursively remove ELEM from TREE using TEST.

Example:
    (pref/remove-from-tree 1 \='(1 2 3 (6 7 8 1 2 3) 1 2 3) #\='=)
    ;; => (2 3 (6 7 8 2 3) 2 3)"
  (cl-labels ((rec (acc e)
                (if (listp e)
                    (cons (pref/remove-from-tree elem e test) acc)
                  (if (funcall test e elem) acc (cons e acc)))))
    (reverse (cl-reduce #'rec tree :initial-value nil))))

(provide 'pref-lib)
;;; pref-lib.el ends here

