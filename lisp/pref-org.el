;;; pref-org --- Configurations for org
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:
(require 'pref-lib)
(require 'org)

(defun pref.inner/org-disable-company-dabbrev ()
  "Disable dabbrev in `org-mode'."
  (when (derived-mode-p 'org-mode)
    (setq-local company-backends
                (pref/remove-from-tree 'company-dabbrev
                                       company-backends
                                       #'eq))))
              
(defun pref.inner/org-mode-init ()
  "Org-mode initial config."
  (visual-line-mode -1)
  (electric-pair-mode -1)
  (display-line-numbers-mode -1)
  (org-indent-mode))

(use-package org :ensure nil
  :hook (org-mode . pref.inner/org-mode-init)
  :init
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook #'pref.inner/org-disable-company-dabbrev)))

;; Treat angle brackets as symbol constituents in Org source blocks
;; See: https://emacs.stackexchange.com/a/52209
;; Adapted from a solution by @john-kitchin.
(defun org-mode-<>-syntax-fix (start end)
  "Mark characters `<' and `>' as symbol constituents inside org src blocks.
This function is suitable for `syntax-propertize-function' and must
only operate on the region from START to END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[<>]" end t)
      (let ((pos (match-beginning 0)))
        (goto-char pos)
        (when (org-in-src-block-p)
          (put-text-property pos (1+ pos) 'syntax-table (string-to-syntax "_")))
        (goto-char (1+ pos))))))

(defun org-setup-<>-syntax-fix ()
  "Set up `syntax-propertize-function' for `org-mode' buffers."
  (setq-local syntax-propertize-function #'org-mode-<>-syntax-fix)
  ;; If you want existing buffer text to be propertized immediately, uncomment:
  ;; (syntax-propertize (point-max))
  )

(add-hook 'org-mode-hook #'org-setup-<>-syntax-fix)

(provide 'pref-org)
;;; pref-org.el ends here

