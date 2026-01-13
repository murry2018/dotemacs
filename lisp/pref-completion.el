;;; pref-completion --- Company and Orderless configuration
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

(use-package company :ensure t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))

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


(declare-function evil-global-set-key "evil")

(with-eval-after-load 'evil
  (evil-global-set-key 'insert (kbd "C-n") #'company-complete)
  (evil-global-set-key 'insert (kbd "C-p") #'company-complete))

(provide 'pref-completion)
;;; pref-completion.el ends here
