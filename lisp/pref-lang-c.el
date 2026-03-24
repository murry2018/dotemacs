;;; pref-lang-c.el --- config for c/c++  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'pref-lib)

(use-package ccls :ensure nil
  ;; :straight nil
  ;; We do not ensure ccls here, as it appears to take precedence
  ;; over clangd if installed.
  :if (locate-library "ccls"))

(use-package lsp-mode
  :hook ((c++-mode c-mode c-ts-mode c++-ts-mode) . lsp)
  :config
  (setopt lsp-enable-on-type-formatting nil
          lsp-enable-indentation nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax-aware Indentation Configuration
;;

;; CC-mode configuration
(c-add-style "pref/c"
             '("bsd"
               (c-basic-offset . 4)
               (c-offsets-alist
                (cpp-macro . 0)
                (arglist-close . c-lineup-close-paren))))
(setopt c-default-style "pref/c")

;; Ts-mode configuration
(defvar c-ts-mode-indent-style)
(declare-function treesit-node-at "treesit" (point))
(declare-function treesit-node-type "treesit.c" (node))
(declare-function c-ts-mode--indent-styles "c-ts-mode" (mode))

(defun pref.lang-c/style ()
  "Define a customized indentation style based on BSD."
  (let* ((mode (if (derived-mode-p 'c-ts-mode) 'c 'c++))
         (bsd-style (alist-get 'bsd (c-ts-mode--indent-styles mode))))
    `(;;; Rules for parameter list for function declaration
      ((match nil "parameter_list" nil 1 1) standalone-parent c-ts-mode-indent-offset)
      ((match ")" "parameter_list" nil nil nil) standalone-parent 0)
      ((match nil "parameter_list" nil 2 nil) (nth-sibling 1) 0)
      ((and no-node (parent-is "parameter_list")) (nth-sibling 1) 0)
      
      ;;; Rules for argument list for function invocation
      ((node-is ")") parent-bol 0)
      ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
      ((parent-is "argument_list") prev-sibling 0)
      ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
      ((parent-is "parameter_list") prev-sibling 0)

      ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "compound_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "if_statement") standalone-parent c-ts-mode-indent-offset)
      
      ;;; Rules for preprocessors conditionals
      ((and (node-is "preproc.*")) no-indent 0)
      ((node-is "#endif") no-indent 0)
      
      ;;; Derived from BSD
      ,@bsd-style)))

(defun pref.lang-c/maybe-literal-tab ()
  "Insert a literal tab for preprocessor directives, otherwise indent."
  (interactive)
  (if (and (eq c-ts-mode-indent-style #'pref.lang-c/style)
           (or (save-excursion (back-to-indentation)
                               (looking-at-p "#"))
               (when-let* ((node (treesit-node-at (point))))
                 (equal (treesit-node-type node)
                        "preproc_arg"))))
      (tab-to-tab-stop)
    (call-interactively #'indent-for-tab-command)))

(defun pref.lang-c/c-ts-mode-hook ()
  "Configure indentation style and keybindings for `c-ts-mode'."
  (setopt c-ts-mode-indent-style #'pref.lang-c/style)
  (local-set-key (kbd "TAB") #'pref.lang-c/maybe-literal-tab)
  (local-set-key (kbd "<tab>") #'pref.lang-c/maybe-literal-tab))

(use-package c-ts-mode
  :defer t
  :hook ((c-ts-mode c++-ts-mode) . pref.lang-c/c-ts-mode-hook)
  :config
  (setopt c-ts-mode-indent-offset 4))

(provide 'pref-lang-c)
;;; pref-lang-c.el ends here
