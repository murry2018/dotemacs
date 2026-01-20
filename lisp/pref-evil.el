;;; pref-evil --- Summary: Configurations for evil-mode
;;; Commentary:
;;
;;; Code:
(declare-function evil-select-search-module "evil-search")

(use-package evil-surround :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil :ensure t
  :init
  (setopt evil-want-keybinding nil
          evil-want-integration t
          evil-want-C-d-scroll t
          evil-want-C-u-scroll t
          display-line-numbers-type 'relative)
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search))

(provide 'pref-evil)
;;; pref-evil.el ends here
