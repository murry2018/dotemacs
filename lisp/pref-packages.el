;;; pref-packages.el --- packages which have simple configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile :ensure t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
          ("C-c p" . projectile-command-map))
  :config
  (setopt projectile-enable-caching t
          projectile-indexing-method 'alien))

;; magit: The standard Git interface for Emacs.
;; (Status: C-x g; Help: '?' inside magit buffer for command list)
(use-package magit :ensure t
  :when (executable-find "git")
  :config
  (setopt magit-define-global-key-bindings 'recommended))

;; wgrep: Edit grep/occur buffers directly and save to files.
;; (Start: C-c C-p, Apply: C-x C-s, Abort: C-c C-k)
(use-package wgrep :ensure t)

;; iedit: Edit all occurrences of a symbol simultaneously.
;; (Toggle: C-; on a target symbol)
(use-package iedit :ensure t
  :if pref/use-iedit)

;; transpose-frame: Swap x/y direction of window splits or rotate them.
;; (M-x transpose-frame, M-x rotate-frame)
(use-package transpose-frame :ensure t)

;; ace-window: Switch windows using visual character hints
;; (Jump: M-o)
(use-package ace-window :ensure t
  :bind ("M-o" . ace-window)
  :demand t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?z ?x ?c ?v))
  (let* ((is-gui (display-graphic-p))
         (font-height (if is-gui 2.5 1.0))
         (fg-color (if is-gui "chartreuse" "green")))
    (set-face-attribute
     'aw-leading-char-face nil
     :foreground fg-color :height font-height :weight 'bold
     :box (when is-gui ; `box' might not work on terminal
            '(:line-width 2 :color "grey75" :style released-button)))))

;; surround: Vim-like surround operations for delimiters (parens, quotes).
;; (Usage: Bind `surround-keymap' to a key to use)
(use-package surround :ensure t)

;; fold-this: Fold the selected region of text.
;; (Fold: M-x fold-this, Unfold: M-x fold-this-unfold-at-point)
(use-package fold-this :ensure t)

;; avy: Moving around the screen quickly
(use-package avy :ensure t
  :bind (("M-'" . #'avy-goto-char-timer)
         ("M-l" . #'avy-goto-line)
         ("M-g c" . #'avy-goto-subword-1)))

;; embark: Emacs native context-menu
(use-package embark :ensure t
  :demand t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setopt prefix-help-command #'embark-prefix-help-command))

;; vterm: terminal implementation based on libvterm
(defun pref.vterm/disable-hl-line-hook ()
  "Disable `hl-line-mode'."
  (when global-hl-line-mode
      (hl-line-mode 'toggle)))

(use-package vterm :ensure t
  :hook (vterm-mode . pref.vterm/disable-hl-line-hook)
  :config
  (setopt vterm-max-scrollback 10000))

(use-package eat :ensure t)

(use-package breadcrumb :ensure t
  :init
  (breadcrumb-mode))

(provide 'pref-packages)
;;; pref-packages.el ends here
