;;; pref-evil.el --- config for evil-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function evil-forward-nearest "evil")
(declare-function evil-forward-chars "evil")
(declare-function forward-evil-empty-line "evil")
(declare-function evil-select-search-module "evil")
(declare-function evil-set-leader "evil")
(declare-function evil-define-key "evil")
(declare-function pref/consult-symbol-search "pref-vertico")

(defvar evil-bigword)
(defvar forward-thing-provider-alist)
(defvar evil-surround-pairs-alist)

(defun pref.evil/forward-evil-WORD (&optional count)
  "Move forward by COUNT `evil-WORD's.
This is a customized version that references `evil-bigword'."
  (evil-forward-nearest count
                        #'(lambda (&optional cnt)
                            (evil-forward-chars evil-bigword cnt))
                        #'forward-evil-empty-line))

(defun pref.evil/forward-evil-WORD-provider (&optional backward)
  "Move forward or backward by one \"WORD\".
If BACKWARD is non-nil, move backward; otherwise, move forward."
  (pref.evil/forward-evil-WORD (if backward -1 1)))

;; -- evil initial configuartion --
(setopt evil-want-integration t
        evil-want-keybinding nil
        evil-symbol-word-search t
        evil-want-fine-undo t
        evil-undo-system 'undo-fu)

(when (require 'evil nil t)
  ;; -- Customize the definition of a WORD --
  (setopt evil-bigword "^]\n\r\t\f ()[{},/") ; default + braces + separator

  (with-eval-after-load "thingatpt"
    ;; `evil-forward-WORD-{end|begin}' internally call ~(forward-thing 'evil-WORD)~.
    ;; This registers custom provider so that `thingatpt' uses it instead of
    ;; falling back to `forward-evil-WORD'.
    (add-to-list 'forward-thing-provider-alist
      '(evil-WORD . pref.evil/forward-evil-WORD-provider)))
  ;; Override the original function to ensure that any direct calls to
  ;; `forward-evil-WORD' also use customized definition.
  ;; (advice-add 'forward-evil-WORD :override #'pref.evil/forward-evil-WORD)

  ;; -- Customize emacs behaviors to fit `evil-mode' --
  (setopt display-line-numbers-type 'relative)
  (electric-pair-mode -1)
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; -- `evil-collection' configuration --
  (require 'evil-collection)
  (setopt evil-collection-magit-want-horizontal-movement t)
  (setopt evil-collection-magit-use-y-for-yank t)
  (setopt evil-collection-magit-visit-worktree-file-on-return nil)
  (evil-collection-init)

  ;; -- `evil-surround' configuration --
  (global-evil-surround-mode 1)
  (let ((alist evil-surround-pairs-alist)
        (open-braces '(?\( ?\[ ?\{))
        (close-braces '(?\) ?\] ?\})))
    (cl-loop
     for open-brace in open-braces
     for close-brace in close-braces
     do (when-let* ((open-cell (alist-get open-brace alist))
                    (close-cell (alist-get close-brace alist)))
          (setf (alist-get open-brace alist) close-cell)
          (setf (alist-get close-brace alist) open-cell))))

  ;; -- Register leader key keymap --
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>x") #'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "<leader>o") #'other-window)
  (evil-define-key 'normal 'global (kbd "<leader>f") #'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>.") #'recentf)
  (evil-define-key 'normal 'global (kbd "<leader>b") #'switch-to-buffer)
  (with-eval-after-load "consult"
    (evil-define-key 'normal 'global (kbd "<leader>.") #'consult-recent-file)
    (evil-define-key 'normal 'global (kbd "<leader>r") #'consult-ripgrep)
    (evil-define-key 'normal 'global (kbd "<leader>b") #'consult-buffer))
  (with-eval-after-load "pref-vertico"
    (evil-define-key 'normal 'global (kbd "<leader>s") #'pref/consult-symbol-search))
  (with-eval-after-load "projectile"
    (evil-define-key 'normal 'global (kbd "<leader>pp") #'projectile-switch-project)
    (evil-define-key 'normal 'global (kbd "<leader>pf") #'projectile-find-file))
  (with-eval-after-load "avy"
    (evil-define-key 'normal 'global (kbd "<leader>'") #'avy-goto-char-timer)
    (evil-define-key 'normal 'global (kbd "f") #'avy-goto-subword-1)
    (evil-define-key 'visual 'global (kbd "f") #'avy-goto-subword-1)
    (evil-define-key 'normal 'global (kbd "F") #'avy-goto-char-timer)
    (evil-define-key 'visual 'global (kbd "F") #'avy-goto-char-timer)))

(provide 'pref-evil)
;;; pref-evil.el ends here
