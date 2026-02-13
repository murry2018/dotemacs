;;; pref-lsp --- Configurations for lsp-mode
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(require 'pref-lib)

(defun pref.inner/disable-flymake-cc ()
  "Disable the legacy `flymake-cc' backend in the current buffer."
  (setq-local flymake-diagnostic-functions
              (delq 'flymake-cc flymake-diagnostic-functions)))

(use-package lsp-mode :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-managed-mode . pref.inner/disable-flymake-cc))
  :config
  (setopt lsp-semantic-tokens-enable t
          lsp-diagnostics-provider :flymake
          lsp-enable-snippet nil)
  :commands lsp)

(use-package lsp-ui :ensure t
  :commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom command `pref-lsp-disable-dir'
;;
;; This command allows you to temporarily disable `lsp-mode' in specific
;; directories, such as when `compile_commands.json' is missing or when
;; developing an LSP server.
;;
;; Note that adding a directory here completely blocks `lsp-mode'
;; activation. Therefore, this should be used for temporary measures only.
;; For permanent exclusion, please use `lsp-session-folders-blocklist'
;; instead. (See the docstring of `pref/*lsp-disable-dirs*' for details).

(defvar pref/*lsp-disable-dirs* nil
  "List of directories where `lsp-mode' should be disabled.

This list is intended for temporary use only. To permanently exclude a
directory, add it to `lsp-session-folders-blocklist' (this can be done
by selecting the \='d' option when `lsp-mode' is first invoked in a
workspace).")

(defvar pref.experiment/*permanent-lsp-disable-dirs* nil
  "Whether to persist `pref/*lsp-disable-dirs*' across sessions.

\(EXPERIMENTAL)
If non-nil, `pref/*lsp-disable-dirs*' is added to
`savehist-additional-variables' to be saved across Emacs restarts.
If nil (default), the list is initialized to nil in each new session.")
(defvar savehist-additional-variables)

(with-eval-after-load 'savehist
  (when pref.experiment/*permanent-lsp-disable-dirs*
    (add-to-list 'savehist-additional-variables 'pref/*lsp-disable-dirs*)))

(defun pref.inner/should-suppress-lsp-p (&rest _args)
  "Return t if the current buffer's file path is in the disable list.
Used as a :before-until advice for `lsp' and `lsp-deferred'."
  (when-let ((name (buffer-file-name)))
    (let ((suppress (cl-some (lambda (dir) (string-prefix-p dir name))
                             pref/*lsp-disable-dirs*)))
      (when suppress
        (message "[PREF] Disabled lsp by pref/*lsp-disable-list* rule."))
      suppress)))

(advice-add 'lsp :before-until #'pref.inner/should-suppress-lsp-p)
(advice-add 'lsp-deferred :before-until #'pref.inner/should-suppress-lsp-p)

(defun pref-lsp-disable-dir ()
  "Prompt for a directory and add it to `pref/*lsp-disable-dirs*`."
  (interactive)
  (let* ((dir (read-directory-name "Disable LSP for directory: "
                                   default-directory nil t))
         (abs-dir (expand-file-name dir)))

    (if (not (file-directory-p abs-dir))
        (message "[PREF] %s is not a directory" abs-dir)

      (let ((final-dir (file-name-as-directory abs-dir)))
        (add-to-list 'pref/*lsp-disable-dirs* final-dir)
        (message "[PREF] Added '%s' to LSP disable list." final-dir)))))

(provide 'pref-lsp)
;;; pref-lsp.el ends here
