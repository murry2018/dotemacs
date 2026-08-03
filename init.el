;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Compatibility packages are sometimes required because maintainers use
;; development versions of Emacs. In these instances, simply install the
;; latest compat package (M-x package-install RET compat RET).
(when (and (version< emacs-version "31")
           (package-installed-p "compat"))
  (require 'compat nil t))

;; Custom file
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; pref scripts
(let ((pref-path (expand-file-name "lisp" user-emacs-directory)))
  (push pref-path load-path))

(require 'pref-config)
(require 'pref-lib)
(require 'pref-default)
(require 'pref-ui)
(require 'pref-non-english)
(require 'pref-tramp)
(require 'pref-completion)
(require 'pref-vertico)
(when pref/use-treesitter
  (require 'pref-treesit))
(when pref/use-evil
  (require 'pref-evil))
(require 'pref-packages)
(require 'pref-langs)

;; This should be the last line
(require 'pref-site-config nil t)

(provide 'init)
;;; init.el ends here
