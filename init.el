;; load-path에 preferences 디렉터리를 추가
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inferior-lisp-program "sbcl")
 '(package-selected-packages
   (quote
    (nov smex ivy highlight-doxygen flycheck-irony company-irony-c-headers company-irony))))
