;; load-path에 preferences 디렉터리를 추가


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let* ((emacs-dir ;; e.g. ~/.emacs.d/
        (file-name-as-directory user-emacs-directory)) 
       (pref-dir ;; e.g. ~/.emacs.d/preferences
        (concat emacs-dir "preferences"))) 
  (if (file-exists-p pref-dir)
      (add-to-list 'load-path pref-dir)))

;; Customize options (theme, font, indent, windmove).
(load "init-customizes")
;; Package initialization
(load "init-packages")

;; automatically added.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (smex ivy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
