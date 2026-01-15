;;; pref-tramp --- Configurations for tramp
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speed up TRAMP
;; See: https://www.gnu.org/software/tramp/#Frequently-Asked-Questions-1
;;

(use-package tramp
  :ensure nil
  :config
  (setopt
      ;; Disable version control to avoid delays
      vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)
      ;; scp is faster than internal method for large files
      tramp-use-scp-direct-remote-copying t
      ;; Disable file locks(t means disable).
      ;; It is safe if different Emacs sessions are not modifying the same remote file.
      remote-file-name-inhibit-locks t
      ;; Disable auto reverting of remote files.
      auto-revert-remote-files nil
      ;; Disable directory-local variables for remote files(default)
      enable-remote-dir-locals nil)
  ;; sudo
  (connection-local-set-profile-variables
   'pref.inner/tramp-profile-sudo
   ;; Disable auto-saving on `sudo' sessions.
   '((buffer-auto-save-file-name . nil)))
  (connection-local-set-profiles
   '(:application tramp :protocol "sudo")
   'pref.inner/tramp-profile-sudo)
  ;; ssh
  (connection-local-set-profile-variables
   'pref.inner/tramp-profile-ssh
   ;; Disable check for symbolic link validity in dired buffers.
   '((dired-check-symlinks . nil)
     ;; Suppress reading the remote history file in shell.
     (shell-history-file-name . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'pref.inner/tramp-profile-ssh))

;; Use a package with TRAMP specific implementation of high-level operations.
(use-package tramp-hlo :ensure t
  :config
  (tramp-hlo-setup))

(provide 'pref-tramp)
;;; pref-tramp.el ends here
