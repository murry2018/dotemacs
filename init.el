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

(require 'pref-default)
(require 'pref-org)
(require 'pref-ivy)
(require 'pref-lsp)
(require 'pref-slime)

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

(use-package magit :ensure t
  :when (executable-find "git")
  :config
  (setopt magit-define-global-key-bindings 'recommended))

(use-package treesit-fold :ensure t
  :init
  (if (fboundp 'global-treesit-fold-indicators-mode)
      (global-treesit-fold-indicators-mode)
    (global-treesit-fold-mode))
  :config
  (setopt treesit-fold-line-count-show t
          treesit-fold-line-count-format " <%d lines> "))

;; This should be the last line
(require 'pref-site-config nil t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
