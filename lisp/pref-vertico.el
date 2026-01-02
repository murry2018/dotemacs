;;; pref-vertico --- Configurations for vertico
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:

(use-package vertico :ensure t
  :ensure t
  :init
  (vertico-mode 1)
  :config
  (setopt vertico-cycle t
          vertico-resize nil))

(use-package marginalia
  :ensure t
  :init
  (setopt marginalia-align 'right)
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         ("C-c g" . consult-ls-git))
  :init
  (keymap-global-set "C-c r"
                     (if (executable-find "rg")
                         #'consult-ripgrep
                       #'consult-grep)))

(use-package orderless :ensure t
  :init
  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil))

(use-package embark :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
            ("C-c C-o" . embark-export)))

(use-package embark-consult :ensure t
  :after embark)

(provide 'pref-vertico)
;;; pref-vertico.el ends here
