;;; pref-keymap.el --- keymap configs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'pref-config)

(declare-function pref/consult-symbol-search "pref-vertico")

(with-eval-after-load "avy"
  (keymap-global-set "M-'" #'avy-goto-char-timer)
  (keymap-global-set "M-l" #'avy-goto-line))
(with-eval-after-load "pref-vertico"
  (keymap-global-set "C-s" #'pref/consult-symbol-search))

(provide 'pref-keymap)
;;; pref-keymap.el ends here
