;;; pref-lib --- functions/macros library  -*- lexical-binding: t -*-
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
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

(defun pref/inverse-cmd (command)
  "Return a command that calls COMMAND with a negative prefix argument."
  #'(lambda ()
      (interactive)
      (let ((current-prefix-arg -1))
        (call-interactively command))))

(defun pref/add-to-alist (scope alist-symbol &rest pairs)
  "Add or replace multiple key/value PAIRS into the alist bound to ALIST-SYMBOL.

SCOPE must be either 'local or 'default, determining which value to modify.
ALIST-SYMBOL must be a symbol whose value is an alist.
PAIRS is a sequence of KEY VALUE arguments.

Example:
  (pref/add-to-alist
   'default
   'evil-surround-pairs-alist
   ?\( '(\"(\" . \")\")
   ?\) '(\"( \" . \" )\"))"
  (unless (memq scope '(local default))
    (error "SCOPE must be either 'local or 'default"))
  (unless (and (symbolp alist-symbol)
               (boundp alist-symbol))
    (error "ALIST-SYMBOL must be a bound symbol"))

  (let ((alist (if (eq scope 'default)
                   (default-value alist-symbol)
                 (symbol-value alist-symbol))))
    
    (while pairs
      (let ((key (pop pairs))
            (val (pop pairs)))
        (setq alist (assq-delete-all key alist))
        (push (cons key val) alist)))
    
    (if (eq scope 'default)
        (set-default alist-symbol alist)
      (set alist-symbol alist))))

(provide 'pref-lib)
;;; pref-lib.el ends here

