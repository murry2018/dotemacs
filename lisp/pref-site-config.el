;;; pref-site-config.el --- config for each site  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-english User Config
;;

(setopt default-input-method "korean-hangul")

(defun pref.site/restore-input-method (orig-fun &rest args)
  "Call ORIG-FUN with ARGS while temporarily deactivating input method.
If `current-input-method' is non-nil, it is deactivated before
calling ORIG-FUN and restored afterwards to ensure that labels
can be selected without input method interference."
  (let ((old-method current-input-method))
    (if old-method
        (unwind-protect
            (progn
              (deactivate-input-method)
              (apply orig-fun args))
          (activate-input-method old-method))
      (apply orig-fun args))))

(defvar pref.site/english-only-commands
  '(ace-window
    avy-goto-line avy-goto-char avy-goto-char-2 avy-goto-char-timer))

(defun pref.site/deactivate-input-method-hook ()
  "Apply input method advice to `pref.site/english-only-commands'."
  (dolist (func pref.site/english-only-commands)
    (advice-add func :around #'pref.site/restore-input-method)))

(add-hook 'after-init-hook #'pref.site/deactivate-input-method-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The other site-specific configs go below...
;;

(use-package color-theme-sanityinc-tomorrow :ensure t)

(provide 'pref-site-config)
;;; pref-site-config.el ends here
