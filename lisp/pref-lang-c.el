;;; pref-lang-c.el --- config for c/c++
;;; Commentary:
;;; Code:
(require 'pref-lib)

(defcustom pref/*clangd-version-minimum* 19
  "Minimum version allowed for clangd (restart required after change).
It is recommended to set this value to 19 or higher. Clangd version
18 (the default on Ubuntu 22.04) is not recommended due to a severe
memory usage issue when encountering specific header file patterns (see
https://github.com/clangd/clangd/issues/719)."
  :type 'number
  :group 'pref)

(defcustom pref/*clangd-command* "clangd"
  "Location of clangd command (restart required after change).
It is recommended to set use clangd version 19 or higher. Clangd version
18 (the default on Ubuntu 22.04) is not recommended due to a severe
memory usage issue when encountering specific header file patterns (see
https://github.com/clangd/clangd/issues/719)."
  :type 'string
  :group 'pref)

(defvar pref.inner/*clangd-version-cmd-result* nil
  "Result of `clangd --version'.")

(defun pref.inner/get-clangd-version-number (version-cmdout)
  "Read major.minor version from VERSION-CMDOUT into number.
Where VERSION-CMDOUT is the result of `clangd --version'."

  (when (and version-cmdout
             (string-match "version \\([0-9]+\\)" version-cmdout))
    (string-to-number (match-string 1 version-cmdout))))

(defun pref.inner/lookup-alternative-clangd (clangd-cmd curr-version)
  "Search for a recommended clangd executable.
Returns CLANGD-CMD if its version (CURR-VERSION) is sufficient
\(i.e., >= pref/*clangd-version-minimum*).

Otherwise, attempts to find a newer version (clangd-20 or clangd-19)
using `executable-find'. Returns nil if no suitable executable is found."

  (if (and curr-version (>= curr-version pref/*clangd-version-minimum*))
      clangd-cmd
    (cl-loop for v from 30 downto pref/*clangd-version-minimum*
             as candidate = (executable-find (format "clangd-%d" v))
             when candidate do (cl-return candidate)
             ;; return nil if no candidate found
             )))

;; for clangd user
(with-eval-after-load 'lsp-mode
  (when-let ((clangd-default (executable-find pref/*clangd-command*)))
    (setf pref.inner/*clangd-version-cmd-result*
          (shell-command-to-string
           (concat (shell-quote-argument clangd-default) " --version")))

    (let* ((version-num (pref.inner/get-clangd-version-number
                         pref.inner/*clangd-version-cmd-result*))
           (good-clangd (pref.inner/lookup-alternative-clangd
                         clangd-default version-num)))
      (setopt lsp-clangd-binary-path (or good-clangd
                                         clangd-default))
      (unless good-clangd
        (warn "It is recommended to set use clangd version 19 or higher.
Clangd version 18 (the default on Ubuntu 22.04) is not recommended
due to a severe memory usage issue when encountering specific header file
patterns (see https://github.com/clangd/clangd/issues/719).")))))

(use-package ccls
  ;; We do not ensure ccls here, as it appears to take precedence
  ;; over clangd if installed.
  :if (locate-library "ccls"))

(use-package lsp-mode
  :hook ((c++-mode c-mode c-ts-mode c++-ts-mode) . lsp))

(c-add-style "pref/c"
             '("k&r"
               (c-basic-offset . 4)
               (c-offsets-alist
                (cpp-macro . 0)
                (arglist-close . c-lineup-close-paren))))
(setopt c-default-style "pref/c")

(use-package c-ts-mode
  :defer t
  :config
  (setopt c-ts-mode-indent-offset 4
          c-ts-mode-indent-style 'k&r))

(provide 'pref-lang-c)
;;; pref-lang-c.el ends here
