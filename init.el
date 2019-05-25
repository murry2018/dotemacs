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
