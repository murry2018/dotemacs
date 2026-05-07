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
    avy-goto-subword-1
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transient keymaps such as repeat-mode use
;; `overriding-terminal-local-map` to read follow-up keys.
;;
;; When an input method is active, Quail intercepts those keys
;; before the transient map can handle them.  This wrapper bypasses
;; input-method translation only for keys that are already bound in
;; the active transient map, while leaving normal text input intact.

(defun pref.noenglish/bypass-for-transient-map (orig-fun key)
  "Call ORIG-FUN with KEY unless KEY is bound in a transient keymap.

If KEY exists in `overriding-terminal-local-map', return the raw
key event directly to bypass input-method translation."
  (let* ((map overriding-terminal-local-map)
         (binding (and map
                       (or (lookup-key map (vector key))
                           (lookup-key map (vector (event-basic-type key)))))))
    (if (and binding (not (numberp binding)))
        (list key)
      (funcall orig-fun key))))

(defun pref.noenglish/install-input-method-hook ()
  "Install transient-map bypass wrapper for the active input method.

Wrap `input-method-function' buffer-locally so transient keymaps
can receive raw key events without Quail translation."
  (when (and input-method-function
             (not (advice-member-p
                   #'pref.noenglish/bypass-for-transient-map
                   input-method-function)))
    (add-function :around (local 'input-method-function)
                  #'pref.noenglish/bypass-for-transient-map)))

(add-hook 'input-method-activate-hook
          #'pref.noenglish/install-input-method-hook)

(provide 'pref-non-english)
;;; pref-non-english.el ends here
