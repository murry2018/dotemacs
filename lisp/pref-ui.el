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
  (global-hl-line-mode -1) ;; too many blinking
  (setopt visible-bell nil)
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

(defun pref.ui/terminal-supports-decscusr-p ()
  "Check if current terminal supports DECSUSR."
  (and (not (display-graphic-p))
       (or
        (getenv "VTE_VERSION")         ; VTE-based terminals(inc. GNOME Term.)
        (member (getenv "TERM_PROGRAM") ; Popular terminal clients
                '("iTerm.app" "WezTerm" "ghostty" "Hyper" "Rio" "Tabby"
                  "Alacritty" "Apple_Terminal"))
        (getenv "KITTY_PID")
        (getenv "KONSOLE_PROFILE_NAME")
        (getenv "WT_SESSION")                 ; Windows Terminal
        (getenv "XTERM_VERSION")
        (getenv "ALACRITTY_WINDOW_ID")
        (string-prefix-p "alacritty" (or (getenv "TERM") ""))
        (string-prefix-p "foot" (or (getenv "TERM") ""))
        (and (string-match-p "xterm\\|rxvt\\|vte\\|gnome\\|screen\\|tmux"
                             (or (getenv "TERM") ""))
             ;; Exclude legacy linux termianls, due to their generally
             ;; restricted cursor control features.
             (not (string-equal (getenv "TERM") "linux"))))))

(defun pref.ui/force-decscusr-advice (&rest _)
  "Send DECSUSR sequence to terminal corresponding to given CURSOR-TYPE."
  (when (and (not (display-graphic-p))
             cursor-type)
    (let* ((type (if (consp cursor-type) (car cursor-type) cursor-type))
           (seq (pcase type
                  ('box  "\e[2 q")   ; steady block
                  ('bar  "\e[6 q")   ; steady bar
                  ('hbar "\e[4 q")   ; steady underline
                  (_     "\e[2 q"))))
      (send-string-to-terminal seq))))

(when (pref.ui/terminal-supports-decscusr-p)
  (setq-default visible-cursor nil)

  (add-variable-watcher 'cursor-type
                        (lambda (_sym _newval op _where)
                          (when (eq op 'set)
                            (pref.ui/force-decscusr-advice))))
  (with-eval-after-load 'evil
    (advice-add 'evil-set-cursor :after
                #'pref.ui/force-decscusr-advice)))

(provide 'pref-ui)
;;; pref-ui.el ends here
