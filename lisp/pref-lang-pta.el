;;; pref-lang-pta.el --- Configs for plain text accounting langs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (executable-find "hledger")
  (use-package hledger-mode :ensure t
    :mode ("\\.hledger\\'" "/\\.hledger\\.journal\\'")))

(provide 'pref-lang-pta)
;;; pref-lang-pta.el ends here
