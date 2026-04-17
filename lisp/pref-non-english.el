;;; pref-non-english.el --- config for non-english user -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setopt default-input-method "korean-hangul")

(defvar pref.noenglish/english-only-commands
  '(;; ace-window
    ace-window
    ;; avy
    avy-goto-line
    avy-goto-symbol-1
    avy-goto-char
    avy-goto-char-2
    avy-goto-char-in-line
    avy-goto-char-timer
    avy-goto-subword-0
    avy-goto-whitespace-end
    ))

(defun pref.noenglish/restore-input-method (orig-fun &rest args)
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

(defun pref.noenglish/deactivate-input-method-hook ()
  "Apply input method advice to `pref.noenglish/english-only-commands'."
  (dolist (func pref.noenglish/english-only-commands)
    (advice-add func :around #'pref.noenglish/restore-input-method)))

(add-hook 'after-init-hook #'pref.noenglish/deactivate-input-method-hook)

(provide 'pref-non-english)
;;; pref-non-english.el ends here
