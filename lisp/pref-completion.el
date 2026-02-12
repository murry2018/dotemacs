;;; pref-completion --- Corfu and Orderless configuration
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom orderless dispatcher
;;

(defun pref.inner/orderless-dispatch-if-bang (pattern _index _total)
  "Match PATTERN literally if it starts with an exclamation mark.

PATTERN is the current search component to examine.
_INDEX is the 0-based index of the component (ignored).
_TOTAL is the total number of components (ignored).

If PATTERN starts with '!', it dispatches to `orderless-literal'
using the remainder of the string."
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-literal . ,(substring pattern 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configurations
;;

(use-package corfu :ensure t
  :init (global-corfu-mode))

(use-package orderless :ensure t
  :init
  (setopt completion-styles '(basic orderless)
          completion-category-defaults nil)
  (add-to-list 'completion-category-overrides
    '(file (styles . (basic partial-completion))))
  (add-to-list 'completion-category-overrides
    '(consult-location (styles . (orderless))))
  (setopt orderless-style-dispatchers
    '(pref.inner/orderless-dispatch-if-bang)))

(provide 'pref-completion)
;;; pref-completion.el ends here
