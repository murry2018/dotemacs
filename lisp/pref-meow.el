;;; pref-meow --- Summary: Configurations for meow
;;; Commentary:
;;
;;; Code:
(declare-function meow-leader-define-key "meow")
(declare-function meow-motion-define-key "meow")
(declare-function meow-normal-define-key "meow")

(defvar pref.inner/*saved-input-method* nil
  "Store the input method active before switching mode.
This variable holds the name of input method(e.g. \"korean-hangul\").")

(defun pref.inner/save-input-method ()
  "Save current input method and disable it."
  (when current-input-method
    (setf pref.inner/*saved-input-method* current-input-method)
    (toggle-input-method)))

(defun pref.inner/load-input-method ()
  "Restore the previously saved input method."
  (when pref.inner/*saved-input-method*
    (set-input-method pref.inner/*saved-input-method*))
  (setf pref.inner/*saved-input-method* nil))

(use-package meow :ensure t
  :hook ((meow-insert-enter . pref.inner/load-input-method)
         (meow-insert-exit . pref.inner/save-input-method))
  :init
  (setopt meow-use-clipboard t
          ;; Vim like scrolling
          scroll-step 1
          scroll-margin 3
          scroll-conservatively 2)
  :config
  (require 'meow)
  (setopt meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (keymap-global-set "C-c w" #'save-buffer)
  (keymap-global-set "C-c q" #'save-buffers-kill-terminal)
  (keymap-global-set "C-c ." #'find-file)
  (when (fboundp 'ace-window)
    (keymap-global-set "C-c o" #'ace-window))
  (when (fboundp 'pref.inner/consult-symbol-search)
    (keymap-global-set "C-c s" #'pref.inner/consult-symbol-search))
  (when (fboundp 'avy-goto-char-timer)
    (keymap-global-set "C-c ;" #'avy-goto-char-timer))
  (with-eval-after-load "surround"
    (defalias 'surround-keymap surround-keymap)
    (meow-normal-define-key '("S" . surround-keymap)))
  (meow-global-mode 1)
  :demand t)

(provide 'pref-meow)
;;; pref-meow.el ends here
