;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Custom file
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; pref scripts
(let ((pref-path (expand-file-name "lisp" user-emacs-directory)))
  (push pref-path load-path)
  (push pref-path elisp-flymake-byte-compile-load-path))

(require 'pref-config)
(require 'pref-lib)
(require 'pref-default)
(require 'pref-ui)
(require 'pref-org)
(require 'pref-tramp)
(require 'pref-completion)
(when pref/*use-ivy*
  (require 'pref-ivy))
(when pref/*use-vertico*
  (require 'pref-vertico))
(when pref/*use-evil*
  (require 'pref-evil))
(when pref/*use-meow*
  (require 'pref-meow))
(when pref/*use-god*
  (use-package god-mode :ensure t
    :bind ("<escape>" . god-local-mode)))
(require 'pref-lsp)
(require 'pref-slime)
(when pref/*use-treesitter*
  (require 'pref-treesit))
(require 'pref-lang-c)
(require 'pref-lang-clj)

(use-package flymake :ensure nil
  :hook (emacs-lisp-mode)
  :commands (flymake-mode))

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

;; wgrep: Edit grep/ivy-occur buffers directly and save to files.
;; (Start: C-c C-p, Apply: C-x C-s, Abort: C-c C-k)
(use-package wgrep :ensure t)

;; iedit: Edit all occurrences of a symbol simultaneously.
;; (Toggle: C-; on a target symbol)
(use-package iedit :ensure t
  :if pref/*use-iedit*)

;; transpose-frame: Swap x/y direction of window splits or rotate them.
;; (M-x transpose-frame, M-x rotate-frame)
(use-package transpose-frame :ensure t)

;; ace-window: Switch windows using visual character hints
;; (Jump: M-o)
(use-package ace-window :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?z ?x ?c ?v)))

;; surround: Vim-like surround operations for delimiters (parens, quotes).
;; (Usage: Bind `surround-keymap' to a key to use)
(use-package surround :ensure t)

;; fold-this: Fold the selected region of text.
;; (Fold: M-x fold-this, Unfold: M-x fold-this-unfold-at-point)
(use-package fold-this :ensure t)

;; This should be the last line
(require 'pref-site-config nil t)

;; Local Variables:
;; flymake-diagnostic-functions: (elisp-flymake-byte-compile)
;; End:
