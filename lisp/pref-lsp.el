;;; pref-lsp --- Configurations for lsp-mode
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(require 'pref-lib)

(use-package lsp-mode :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-moe . lsp-enable-which-key-integeration)
  :config
  (setopt lsp-semantic-tokens-enable t)
  :commands lsp)

(use-package lsp-ui :ensure t
  :commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy integration
;;

(use-package lsp-ivy :ensure t
  :after ivy
  :commands lsp-ivy-workspace-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy integration - update headerline within swiper
;;
;; When `swiper' is active, focus moves to the minibuffer, preventing the
;; lsp-headerline update function (`lsp-headerline-check-breadcrumb') from
;; being called. Therefore, we force the update function to run on every
;; command (`post-command-hook') while `swiper' is running.

(declare-function lsp-headerline-check-breadcrumb "lsp-headerline")

(defun pref/swiper-update-lsp-breadcrumb ()
  "Update LSP Breadcrumb while using swiper."
  (when (and (bound-and-true-p ivy-mode)
             (eq (ivy-state-caller ivy-last) 'swiper))
    (let ((source-window (ivy-state-window ivy-last)))
      (when (and (window-live-p source-window)
                 (bound-and-true-p lsp-mode))
        (with-selected-window source-window
          (lsp-headerline-check-breadcrumb))))))

(defun pref/swiper-breadcrumb-setup ()
  "Set up local hook for swiper breadcrumb update."
  (add-hook 'post-command-hook #'pref/swiper-update-lsp-breadcrumb nil t))

(with-eval-after-load 'ivy
  ;; If performance issues arise with swiper later, it is recommended to
  ;; remove this hook first.
  (add-hook 'minibuffer-setup-hook #'pref/swiper-breadcrumb-setup))

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
