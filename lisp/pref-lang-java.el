;;; pref-lang-java.el --- config for java  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'pref-config)
(require 'treesit)

(add-to-list 'treesit-language-source-alist
             '(java "https://github.com/tree-sitter/tree-sitter-java"))

(use-package eglot
  :hook ((java-mode java-ts-mode) . eglot-ensure))

(defun pref.java/ts-indent-config-init-hook ()
  "Fix indentation rules for try-with-resources in java-ts-mode for Emacs 30."
  (setq-local treesit-simple-indent-rules
              `((java
                 ((parent-is "resource_specification") (nth-sibling 1) 0)
                 ,@(alist-get 'java treesit-simple-indent-rules)))))

(defun pref.java/indent-config-init-hook ()
  "Configuration for cc-mode(java-mode) indentation rule."
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont '+)
  (c-set-offset 'arglist-close 0))

(if pref/use-java-ts-mode
    (progn
      (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
      (setopt java-ts-mode-indent-offset 4)
      (add-hook 'java-ts-mode-hook #'pref.java/ts-indent-config-init-hook))
  (add-hook 'java-mode-hook #'pref.java/indent-config-init-hook))

(provide 'pref-lang-java)
;; pref-lang-java.el ends here
