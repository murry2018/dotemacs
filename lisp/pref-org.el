;;; pref-org --- Configurations for org
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(require 'pref-lib)
(require 'org)

(defun pref.inner/org-disable-company-dabbrev ()
  "Disable dabbrev in `org-mode'."
  (when (derived-mode-p 'org-mode)
    (setq-local company-backends
                (pref/remove-from-tree 'company-dabbrev
                                       company-backends
                                       #'eq))))
              
(defun pref.inner/org-mode-init ()
  "Org-mode initial config."
  (visual-line-mode -1)
  (electric-pair-mode -1)
  (display-line-numbers-mode -1)
  (org-indent-mode))

(use-package org :ensure nil
  :hook (org-mode . pref.inner/org-mode-init)
  :init
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook #'pref.inner/org-disable-company-dabbrev)))

(provide 'pref-org)
;;; pref-org.el ends here

