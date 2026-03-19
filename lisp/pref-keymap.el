;;; pref-keymap.el --- keymap configs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'pref-config)

(use-package hydra :ensure t)

(keymap-global-set "C-s" #'isearch-forward)
(with-eval-after-load "avy"
  (keymap-global-set "M-'" #'avy-goto-char-timer)
  (keymap-global-set "M-l" #'avy-goto-line))
(with-eval-after-load "pref-vertico"
  (keymap-global-set "C-c s" #'pref.inner/consult-symbol-search))

(when pref.non-evil/*use-movement-hydra*
  (defhydra hydra-movement (global-map "M-m" :foreign-keys warn)
    "movement"
    ("n" next-line "Down" :column "Cursor")
    ("p" previous-line "Up" :column "Cursor")
    ("f" forward-char "Right" :column "Cursor")
    ("b" backward-char "Left" :column "Cursor")
    ("v" scroll-up-command "Scroll up" :column "Cursor")
    ("l" recenter-top-bottom "Recenter" :column "Cursor")
    ("<" beginning-of-buffer "Buffer begin" :column "Text object")
    (">" end-of-buffer "Buffer end" :column "Text object")
    ("a" move-beginning-of-line "Line begin" :column "Text object")
    ("e" move-end-of-line "Line end" :column "Text object")
    ("C-f" forward-sexp "Sexp forward" :column "Text object")
    ("C-b" backward-sexp "Sexp backward" :column "Text object")
    ("gl" avy-goto-line "Line" :column "Goto")
    ("gc" avy-goto-char "Char" :column "Goto")
    ("gg" avy-goto-char-timer "Timer" :column "Goto")
    ("." xref-find-definitions "Definition" :column "Goto")
    ("," xref-go-back "Go back" :column "Goto")
    ("d" delete-char "Del Char" :column "Edit")
    ("s" (lambda ()
           (interactive)
           (isearch-forward)
           (hydra-movement/body))
     "Search" :column "Action" :color blue)
    ("r" (lambda ()
           (interactive)
           (isearch-backward)
           (hydra-movement/body))
     "Reverse" :column "Action" :color blue)
    ("!" undo "Undo" :column "Action")
    ("RET" nil "quit" :column "Action")
    ("M-m" nil "quit" :column "Action")))

(provide 'pref-keymap)
;;; pref-keymap.el ends here
