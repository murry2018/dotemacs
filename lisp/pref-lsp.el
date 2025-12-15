;;; pref-lsp --- Configurations for lsp-mode
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(use-package lsp-mode :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (((c++-mode c-mode c-ts-mode c++-ts-mode) . lsp)
         (lsp-moe . lsp-enable-which-key-integeration))
  :config
  (setopt lsp-semantic-tokens-enable t)
  :commands lsp)

(use-package lsp-ui :ensure t
  :commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy integration
;;

(use-package lsp-ivy :ensure t
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
      (when (and (window-live-p source-window))
        (with-selected-window source-window
          (lsp-headerline-check-breadcrumb))))))

(defun pref/swiper-breadcrumb-setup ()
  "Set up local hook for swiper breadcrumb update."
  (add-hook 'post-command-hook #'pref/swiper-update-lsp-breadcrumb nil t))

(with-eval-after-load 'ivy
  ;; If performance issues arise with swiper later, it is recommended to
  ;; remove this hook first.
  (add-hook 'minibuffer-setup-hook #'pref/swiper-breadcrumb-setup))

(provide 'pref-lsp)
;;; pref-lsp.el ends here
