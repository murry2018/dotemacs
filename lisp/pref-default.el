;;; pref-default --- Better, modernized defaults
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Basics
;;

;; Set fill-column and turn indicator on
(setopt fill-column 78)			; default: 72
(global-display-fill-column-indicator-mode)

;; Turn off scrollbar and toolbar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  ;; Disable menu-bar-mode in terminal mode.
  ;; Since F10 is often captured by terminal emulator.
  (menu-bar-mode (if (display-graphic-p) 1 -1)))

;; Frame background (for dark background terminal)
;;; The default setting is 'light, which doesn't fit since most modern terminal
;;; emulators use a dark background.
(when (not (display-graphic-p))
  (setopt frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list)))

;; Elisp appearance
;;; These functions (or keywords) are indented *as if they were macros* because
;;; their arguments or argument lists are typically long, often necessitating
;;; a line break.
(put 'setopt 'lisp-indent-function 1)
(put 'add-hook 'lisp-indent-function 1)
(put 'add-to-list 'lisp-indent-function 1)
(put ':map 'lisp-indent-function 1)
(put 'advice-add 'lisp-indent-function 1)

;; Encoding
(prefer-coding-system 'utf-8)		; default: system-dependent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Modes
;;

;; Use `ibuffer' instead of `list-buffers'
;;; A more powerful buffer manager with keybindings and filters
(keymap-global-set "C-x C-b" 'ibuffer)

;; Use `display-line-numbers-mode' only on file-backed buffers
(defun pref/enable-line-numbers ()
  "Use `display-line-numbers-mode' only on file-backed buffers."
  (when buffer-file-name
    (display-line-numbers-mode 1)))

(add-hook 'after-change-major-mode-hook
  #'pref/enable-line-numbers)

;; Use `show-paren-mode'
;;; Highlights matching parentheses/brackets/braces
(show-paren-mode 1)
(setopt show-paren-when-point-inside-paren t)

;; Use `save-place-mode'
;;; Remembers the last cursor position
(save-place-mode 1)

;; Use `recentf-mode'
;;; Remembers recently opened files
;;; (affects functions like find-file, switch-to-buffer, etc.)
(recentf-mode 1)

;; Use `savehist-mode'
;;; Remembers minibuffer history
(savehist-mode 1)

;; Use `global-subword-mode'
;;; Allows movement across CamelCase words
(global-subword-mode 1)

;; Don't `blink-cursor-mode'
;;; Don't blink
(blink-cursor-mode -1)

;; Use `eldoc-mode' in `ielm-mode'
;;; Displays information about Emacs Lisp functions or variables
;;; in the echo area
(add-hook 'ielm-mode-hook 'eldoc-mode)

;; Tab line
(global-tab-line-mode -1)

;; Highlight line
(global-hl-line-mode 1)

;; Use `dired-extra'
;;; Activates `dired-do-*' commands
(require 'dired-x)

;; Which-key mode
;;; Shows a pop-up with key completions after a prefix key
(which-key-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Variables
;;
(setopt
    ;; Ask y/n instead of yes/no in prompts.
    use-short-answers t
    
    ;; Set tab width to 4.
    tab-width 4
    c-basic-offset 4
    
    ;; Automatically switch focus to the help window when it opens.
    help-window-select t
    
    ;; Treat sentences as ending with a period and a single space.
    ;; This affects sentence navigation commands like M-e (forward-sentence)
    ;; and M-a (backward-sentence).
    sentence-end-without-period nil
    sentence-end-double-space nil
    
    ;; Make mouse yank commands paste at the current point, not the mouse
    ;; click position.
    mouse-yank-at-point t
    
    ;; Draw the block cursor as wide as the character it covers.
    x-stretch-cursor t
    
    ;; Use the 'forward' style for unique buffer names (e.g., 'dir/file').
    ;; Instead of the 'suffix' style (e.g., 'file<dir>').
    uniquify-buffer-name-style 'forward
    
    ;; Resize window combinations proportionally when changing size.
    window-combination-resize t

    ;; Prevent indentation from inserting actual TAB characters (use spaces).
    indent-tabs-mode nil

    ;; List directories first when using Dired (ls-lisp-mode).
    ls-lisp-dirs-first t

    ;; Flash the screen (visual bell) instead of producing an audible beep.
    visible-bell t

    ;; Allow recursive use of the minibuffer
    ;; e.g. calling `M-x' or `describe-variable' while inside `find-file'
    enable-recursive-minibuffers t
    )

(provide 'pref-default)
;;; pref-default.el ends here
