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

(use-package expand-region :ensure t
  :config
  (setopt er/try-expand-list
    '(er/mark-inside-quotes
      er/mark-outside-quotes
      er/mark-inside-pairs
      er/mark-outside-pairs)))

(use-package meow :ensure t
  :hook ((meow-insert-enter . pref.inner/load-input-method)
         (meow-insert-exit . pref.inner/save-input-method))
  :init
  (setopt meow-use-clipboard t
          ;; Vim like scrolling
          ;; scroll-step 1
          ;; scroll-margin 3
          scroll-conservatively 5
          )
  :config
  (require 'meow)
  (setopt meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))

  (setopt meow-char-thing-table
    '((?\( . round)
      (?\[ . square)
      (?\{ . curly)
      (?< . angle)
      (?\" . string)
      (?p . paragraph)
      (?l . line)
      (?b . buffer)))

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
    '("q" . meow-quit)
    '("-" . negative-argument)

                                        ; movement
    '("i" . meow-prev)
    '("k" . meow-next)
    '("j" . meow-left)
    '("l" . meow-right)

    '("n" . meow-search)
    '("/" . meow-visit)
    `("?" . ,(pref/inverse-cmd #'meow-visit))

                                        ; expansion by text object
    '("I" . meow-prev-expand)
    '("K" . meow-next-expand)
    '("J" . meow-left-expand)
    '("L" . meow-right-expand)

    '("h" . meow-back-word)
    '("H" . meow-back-symbol)
    '(";" . meow-next-word)
    '(":" . meow-next-symbol)

    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    '("b" . meow-block)
    '("m" . meow-line)
    '("a" . meow-join)
    '("s" . meow-grab)
    ;; '("Z" . meow-pop-grab)
    '("G" . meow-cancel-selection)
    '("g" . meow-pop-selection)

    '("t" . meow-till)
    `("T" . meow-till-expand)
    '("f" . meow-find)
    `("F" . meow-find-expand)

    '("<" . meow-beginning-of-thing)
    '(">" . meow-end-of-thing)
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)

                                        ; editing
    '("d" . meow-kill)
    '("c" . meow-change)
    '("x" . meow-delete)
    '("X" . meow-backward-delete)
    '("y" . meow-save)
    '("p" . meow-yank)
    '("P" . meow-yank-pop)

    '("e" . meow-insert)
    '("r" . meow-append)
    '("o" . meow-open-below)
    '("O" . meow-open-above)

    '("u" . undo-only)
    '("U" . undo-redo)

    '("[" . indent-rigidly-left-to-tab-stop)
    '("]" . indent-rigidly-right-to-tab-stop)

    ;; ignore escape
    '("<escape>" . ignore))
  (when (fboundp 'ace-window)
    (keymap-global-set "C-c o" #'ace-window))
  (when (fboundp 'pref.inner/consult-symbol-search)
    (keymap-global-set "C-c s" #'pref.inner/consult-symbol-search))
  (when (fboundp 'avy-goto-char-timer)
    (meow-normal-define-key '("z" . avy-goto-char-2))
    (keymap-global-set "C-c ;" #'avy-goto-char-timer))
  (with-eval-after-load "surround"
    (defalias 'surround-keymap surround-keymap)
    (meow-normal-define-key '("S" . surround-keymap)))
  (with-eval-after-load "expand-region"
    (meow-normal-define-key '("b" . er/expand-region)))
  (meow-global-mode 1)
  :demand t)

(provide 'pref-meow)
;;; pref-meow.el ends here
