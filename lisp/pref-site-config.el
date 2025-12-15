;;; pref-site-config.el --- config for each site
;;; Commentary:
;;; Code:
(setopt default-input-method "korean-hangul")
(set-frame-font "IBM Plex Mono-11")

(use-package treesit
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(provide 'pref-site-config)
;;; pref-site-config.el ends here
