;;; pref-ivy --- Configurations for ivy
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(use-package ivy :ensure t
  :hook (after-init . ivy-mode)
  :bind ("C-c C-r" . ivy-resume)
  :bind (:map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ("C-l" . counsel-up-directory))
  :bind (:map ivy-occur-grep-mode-map
          ("C-n" . next-error)
          ("C-p" . previous-error))
  :config
  (setopt ivy-use-virtual-buffers 'recentf
        ivy-magic-tilde nil
        ;; Do not close minibuffer when del pressed with empty minibuffer
        ivy-on-del-error-function #'ignore))

(defun pref.inner/find-fd-executable ()
  "Return the path to the `fd' executable.
Checks for both `fd' and `fdfind' (used in some Linux distros)."
  (or (executable-find "fd")
      (executable-find "fdfind")))

(defun pref.inner/counsel-everything ()
  "Search all files in the current directory using `find'."
  (interactive)
  (let* ((cands (split-string
                     (shell-command-to-string "find .") "\n" t)))
        (ivy-read "File: " cands
                  :action #'find-file
                  :caller 'fhd/counsel-everything)))

(use-package counsel :ensure t
  :hook (after-init . counsel-mode)
  :bind (("C-x b" . counsel-switch-buffer)
         ("C-c g" . counsel-git)
         ("C-c F" . pref.inner/counsel-everything)
         ("C-c j" . counsel-git-grep)
         ("C-c r" . counsel-rg)))

(use-package counsel-fd :ensure t
  :after counsel
  :bind (("C-c f" . counsel-fd-file-jump))
  :config
  (when-let ((fd-bin (pref.inner/find-fd-executable)))
    (setopt counsel-fd-command
      (format "%s --hidden --color never " fd-bin))))

(use-package swiper :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-M-s" . swiper-all)))

(provide 'pref-ivy)
;;; pref-ivy.el ends here
