;;; pref-centaur-tabs --- Configuration for `centaur-tabs'
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(use-package centaur-tabs :ensure t
  :bind (("C-S-<tab>" . centaur-tabs-backward)
         ("C-<iso-lefttab>" . centaur-tabs-backward)
         ("C-<tab>" . centaur-tabs-forward))
  :demand
  :config
  (setopt centaur-tabs-cycle-scope 'tabs
          centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-mode t))

(use-package ivy
  :after centaur-tabs
  :bind ("C-c t" . centaur-tabs-counsel-switch-group))

(provide 'pref-centaur-tabs)
;;; pref-centaur-tabs.el ends here

