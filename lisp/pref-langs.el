;;; pref-langs.el --- Configurations for programming languages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; LSP
(require 'pref-lsp)

;; clojure
(use-package cider :ensure t)

;; hledger
(when (executable-find "hledger")
  (use-package hledger-mode :ensure t
    :mode ("\\.hledger\\'" "/\\.hledger\\.journal\\'")))

;; C/C++
(require 'pref-lang-c)

;; Lisp
(require 'pref-slime)

;; Org-mode
(require 'pref-org)

(provide 'pref-langs)
;;; pref-langs.el ends here
