;;; pref-ui.el --- config for basic UI  -*- lexical-binding: t -*-
;;; Author: JY Lee
;;; Commentary:
;;; Code:

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

;; Use `display-line-numbers-mode' only on file-backed buffers
(defun pref.ui/line-numbers-hook ()
  "Use `display-line-numbers-mode' only on file-backed buffers."
  (when buffer-file-name
    (display-line-numbers-mode 1)))

(add-hook 'after-change-major-mode-hook
  #'pref.ui/line-numbers-hook)

;; Frame fonts setup
(defun pref.ui/set-font-variable (symbol value)
  "Set SYMBOL to VALUE and update font on all frames.
This function is used as a setter for font variables."
  (set-default symbol value)
  (when (fboundp 'pref.ui/after-make-frame-hook)
    (mapc #'pref.ui/after-make-frame-hook (frame-list))
    t))

(defcustom pref/font-family "IBM Plex Mono"
  "The font family name to be used for frames."
  :type 'string
  :group 'pref
  :set #'pref.ui/set-font-variable)

(defcustom pref/font-size 11
  "The font size to be used for frames."
  :type 'integer
  :group 'pref
  :set #'pref.ui/set-font-variable)

(defun pref.ui/make-font-name ()
  "Return the formatted font string based on current settings.
Combines `pref/font-family' and `pref/font-size'."
  (format "%s-%d" pref/font-family pref/font-size))

(defun pref.ui/x-font-available-p (font-family)
  "Return non-nil if FONT-FAMILY is available on system."
  (member font-family (font-family-list)))

(defun pref.ui/after-make-frame-hook (frame)
  "Apply the configured font settings to FRAME."
  (with-selected-frame frame
    (when (pref.ui/x-font-available-p pref/font-family)
      (set-frame-font (pref.ui/make-font-name) nil t))))

(mapc #'pref.ui/after-make-frame-hook (frame-list))
(add-hook 'after-make-frame-functions #'pref.ui/after-make-frame-hook)

(provide 'pref-ui)
;;; pref-ui.el ends here
