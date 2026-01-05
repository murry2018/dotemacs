;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Custom file
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; pref scripts
(push (expand-file-name "lisp" user-emacs-directory)
      load-path)

(require 'pref-config)
(require 'pref-lib)
(require 'pref-default)
(require 'pref-org)
(when pref/*use-ivy*
  (require 'pref-ivy))
(when pref/*use-vertico*
  (require 'pref-vertico))
(require 'pref-evil)
(require 'pref-lsp)
(require 'pref-slime)
(require 'pref-treesit)
(require 'pref-lang-c)

(use-package company :ensure t
  :hook (after-init . global-company-mode))

(use-package flycheck :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  ;; Make Flycheck aware of Emacs Lisp files found within the current
  ;; `load-path'. This prevents false warnings when checking Elisp code
  ;; that uses 'require'.
  (setopt flycheck-emacs-lisp-load-path 'inherit))

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
(use-package iedit :ensure t)

;; transpose-frame: Swap x/y direction of window splits or rotate them.
;; (M-x transpose-frame, M-x rotate-frame)
(use-package transpose-frame :ensure t)

(use-package ace-window :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?z ?x ?c ?v)))

;; This should be the last line
(require 'pref-site-config nil t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
