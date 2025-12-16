;;; pref-helm --- Configurations for helm
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(use-package helm :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x r l" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-s" . helm-occur)
         ("C-r" . helm-occur)
         ("C-c g" . helm-ls-git)
         ("C-c j" . helm-grep-do-git-grep)
         ("C-c C-r" . helm-resume))
  :hook (after-init . helm-mode)
  :config (setopt helm-split-window-inside-p t))

(use-package helm-rg :ensure t
  :bind ("C-c r" . helm-rg))

(use-package helm-ls-git :ensure t)

(use-package helm-projectile :ensure t)

(provide 'pref-helm)
;;; pref-helm.el ends here
