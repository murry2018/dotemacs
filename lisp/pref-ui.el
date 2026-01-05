;;; pref-ui.el --- config for basic UI
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
(defun pref/enable-line-numbers ()
  "Use `display-line-numbers-mode' only on file-backed buffers."
  (when buffer-file-name
    (display-line-numbers-mode 1)))

(add-hook 'after-change-major-mode-hook
  #'pref/enable-line-numbers)

;; Frame fonts setup
(defun pref.inner/update-font-on-set (symbol value)
  "Set SYMBOL to VALUE and update font on all frames.
This function is used as a setter for font variables."
  (set-default symbol value)
  (when (fboundp 'pref.inner/after-make-frame)
    (mapc #'pref.inner/after-make-frame (frame-list))
    t))

(defcustom pref/*font-family* "IBM Plex Mono"
  "The font family name to be used for frames."
  :type 'string
  :group 'pref
  :set #'pref.inner/update-font-on-set)

(defcustom pref/*font-size* 11
  "The font size to be used for frames."
  :type 'integer
  :group 'pref
  :set #'pref.inner/update-font-on-set)

(defun pref.inner/make-font-name ()
  "Return the formatted font string based on current settings.
Combines `pref/*font-family*' and `pref/*font-size*'."
  (format "%s-%d" pref/*font-family* pref/*font-size*))

(defun pref.inner/x-font-available-p (font-family)
  "Return non-nil if FONT-FAMILY is available on system."
  (member font-family (font-family-list)))

(defun pref.inner/after-make-frame (frame)
  "Apply the configured font settings to FRAME."
  (with-selected-frame frame
    (when (pref.inner/x-font-available-p pref/*font-family*)
      (set-frame-font (pref.inner/make-font-name) nil t))))

(mapc #'pref.inner/after-make-frame (frame-list))
(add-hook 'after-make-frame-functions #'pref.inner/after-make-frame)

(provide 'pref-ui)
;;; pref-ui.el ends here
