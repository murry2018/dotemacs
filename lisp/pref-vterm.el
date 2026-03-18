;;; pref-vterm.el --- config for vterm.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun pref.inner/disable-hl-line-mode ()
  "Disable `hl-line-mode'."
  (when global-hl-line-mode
      (hl-line-mode 'toggle)))

(use-package vterm :ensure t
  :hook (vterm-mode . pref.inner/disable-hl-line-mode)
  :config
  (setopt vterm-max-scrollback 10000))

(provide 'pref-vterm)
;;; pref-vterm.el ends here
