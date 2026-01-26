;;; pref-meow --- Summary: Configurations for meow
;;; Commentary:
;;
;;; Code:
(require 'pref-lib)

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
  (meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))

  (setopt meow-char-thing-table
    '((?f . round)
      (?d . square)
      (?s . curly)
      (?a . angle)
      (?r . string)
      (?v . paragraph)
      (?c . line)
      (?x . buffer)))

  (meow-motion-define-key
   '("<escape>" . ignore))
  (meow-define-keys 'normal
                                        ; expansion
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)
    '("'" . meow-reverse)

                                        ; movement
    '("i" . meow-prev)
    '("k" . meow-next)
    '("j" . meow-left)
    '("l" . meow-right)

    '("y" . meow-search)
    '("/" . meow-visit)
    `("?" . ,(pref/inverse-cmd #'meow-visit))

                                        ; expansion by text object
    '("I" . meow-prev-expand)
    '("K" . meow-next-expand)
    '("J" . meow-left-expand)
    '("L" . meow-right-expand)

    '("u" . meow-back-word)
    '("U" . meow-back-symbol)
    '("o" . meow-next-word)
    '("O" . meow-next-symbol)

    '("a" . meow-mark-word)
    '("A" . meow-mark-symbol)
    '("s" . meow-line)
    '("S" . meow-goto-line)
    '("w" . meow-block)
    '("q" . meow-join)
    '("g" . meow-grab)
    '("G" . meow-pop-grab)
    '("m" . meow-swap-grab)
    '("M" . meow-sync-grab)
    '("p" . meow-cancel-selection)
    '("P" . meow-pop-selection)

    '("x" . meow-till)
    `("X" . ,(pref/inverse-cmd #'meow-till))
    '("z" . meow-find)
    `("Z" . ,(pref/inverse-cmd #'meow-find))

    '("," . meow-beginning-of-thing)
    '("." . meow-end-of-thing)
    '("<" . meow-inner-of-thing)
    '(">" . meow-bounds-of-thing)

                                        ; editing
    '("d" . meow-kill)
    '("f" . meow-change)
    '("t" . meow-delete)
    '("T" . meow-backspace)
    '("c" . meow-save)
    '("v" . meow-yank)
    '("V" . meow-yank-pop)

    '("e" . meow-insert)
    '("E" . meow-open-above)
    '("r" . meow-append)
    '("R" . meow-open-below)

    '("h" . undo-only)
    '("H" . undo-redo)

    '("b" . open-line)
    '("B" . split-line)

    '("[" . indent-rigidly-left-to-tab-stop)
    '("]" . indent-rigidly-right-to-tab-stop)

                                        ; prefix n
    '("nf" . meow-comment)
    '("nr" . kmacro-start-macro-or-insert-counter)
    '("ne" . kmacro-end-or-call-macro)
    ;; ...etc

                                        ; prefix ;
    '(";F" . save-some-buffers)
    '(";/" . meow-query-replace-regexp)
    '(";w" . save-buffer)
    '(";q" . quit-window)
    '(";j" . recenter-top-bottom)
    ;; ... etc

    ;; ignore escape
    '("<escape>" . ignore))
  (when (fboundp 'ace-window)
    (keymap-global-set "C-c o" #'ace-window))
  (when (fboundp 'pref.inner/consult-symbol-search)
    (keymap-global-set "C-c s" #'pref.inner/consult-symbol-search))
  (when (fboundp 'avy-goto-char-timer)
    (keymap-global-set "C-c ;" #'avy-goto-char-timer))
  (with-eval-after-load "surround"
    (defalias 'surround-keymap surround-keymap)
    (meow-normal-define-key '(":" . surround-keymap)))
  (meow-global-mode 1)
  :demand t)

(provide 'pref-meow)
;;; pref-meow.el ends here
