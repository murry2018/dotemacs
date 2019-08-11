;; Theme setting
(load-theme 'tango-dark)

;; CJK font config on gnu/linux system
(when (and (eq system-type 'gnu/linux)
           (display-graphic-p))
  (if (and (x-list-fonts "D2Coding") (x-list-fonts "Noto Mono"))
      (progn
        (set-face-attribute 'default nil :family "Noto Mono")
        (set-face-attribute 'default nil :height 110)
        (set-fontset-font t 'hangul (font-spec :name "D2Coding"))
        (setq face-font-rescale-alist
              '(("D2Coding" . 1.21))))
    (warn "Checking fonts 'D2Coding' & 'Noto Mono' failed")))

;; No Tabs, use spaces
(setq-default indent-tabs-mode nil)

;; No menu bars
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;; Allow to move point from window to window using Shift + Arrow.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; sentences end with single space (setting for M-a M-e)
(setq sentence-end-double-space nil)
