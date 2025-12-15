;;; pref-ivy --- Configurations for ivy
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(use-package ivy :ensure t
  :hook (after-init . ivy-mode)
  :bind ("C-c C-r" . ivy-resume)
  :bind (:map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ("M-j" . ivy-yank-symbol))
  :bind (:map ivy-occur-grep-mode-map
         ("C-n" . next-error-no-select)
         ("C-p" . previous-error-no-select))
  :config
  (setopt ivy-use-virtual-buffers 'recentf
        ivy-magic-tilde nil
        ;; Do not close minibuffer when del pressed with empty minibuffer
        ivy-on-del-error-function #'ignore))

(use-package counsel :ensure t
  :hook (after-init . counsel-mode)
  :bind (("C-x b" . counsel-switch-buffer)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c r" . counsel-rg)))

(use-package swiper :ensure t
  :bind (("C-s" . swiper)
         ("C-M-s" . swiper-all)))

(provide 'pref-ivy)
;;; pref-ivy.el ends here
