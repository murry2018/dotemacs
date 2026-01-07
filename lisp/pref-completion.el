;;; pref-completion --- Company and Orderless configuration
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:

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
    '(consult-location (styles . (orderless)))))

(declare-function evil-global-set-key "evil")

(with-eval-after-load 'evil
  (evil-global-set-key 'insert (kbd "C-n") #'company-complete)
  (evil-global-set-key 'insert (kbd "C-p") #'company-complete))

(provide 'pref-completion)
;;; pref-completion.el ends here
