;;; pref-keymap.el --- keymap configs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'pref-lib)
(require 'pref-config)

(declare-function pref/consult-symbol-search "pref-vertico")

(with-eval-after-load "avy"
  (keymap-global-set "M-'" #'avy-goto-char-timer)
  (keymap-global-set "M-l" #'avy-goto-line))
(with-eval-after-load "pref-vertico"
  (keymap-global-set "C-s" #'pref/consult-symbol-search))

(repeat-mode 1)
(setopt repeat-exit-timeout 5)
(defvar motion-repeat-map (make-sparse-keymap))

(defvar motion-keys
  `(("f" ,#'forward-char)
    ("b" ,#'backward-char)
    ("p" ,#'previous-line)
    ("n" ,#'next-line)
    ("F" ,#'forward-sexp)
    ("B" ,#'backward-sexp)
    ("a" ,#'move-beginning-of-line)
    ("e" ,#'move-end-of-line)
    ([M-f] ,#'forward-word)
    ([M-b] ,#'backward-word)
    (">" ,#'end-of-buffer)
    ("<" ,#'beginning-of-buffer)
    ("g" ,#'goto-line)
    (,(kbd "C-SPC") ,#'set-mark-command)
    ))

(defun pref.keymap/add-motion-repeat-key (key cmd)
  "Add given KEY / CMD pair to `motion-repeat-map'."
  (define-key motion-repeat-map key cmd)
  (put cmd 'repeat-map 'motion-repeat-map))

(dolist (key-cmd-pair motion-keys)
  (let* ((key (pref/fst key-cmd-pair))
         (cmd (pref/snd key-cmd-pair)))
    (pref.keymap/add-motion-repeat-key key cmd)))

(with-eval-after-load "avy"
  (pref.keymap/add-motion-repeat-key "'" #'avy-goto-subword-1)
  (pref.keymap/add-motion-repeat-key "l" #'avy-goto-line))

(provide 'pref-keymap)
;;; pref-keymap.el ends here
