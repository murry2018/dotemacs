;;; pref-slime --- SLIME configuration
;;; Author: JY Lee
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preface(Managing lisp implementations)
;;

(defconst *pref-lisp-impls*
  '(( :prog "sbcl"
      :implementation (sbcl ("sbcl")
                            :coding-system utf-8-unix))
    ( :prog "qlot"
      :implementation (qlot ("qlot" "exec" "sbcl")
                            :coding-system utf-8-unix))
    ( :prog "ros"
      :implementation (ros ("ros" "-Q" "run")
                           :coding-system utf-8-unix))))

(defun pref.inner/resolve-slime-impls (impl-list)
  "Make a `slime-lisp-implementations' list from given IMPL-LIST.
Filter out programs(`:prog' member) that are not executable."
  (let ((valid-impls '()))
    (dolist (item impl-list)
      (cl-destructuring-bind (&key prog implementation) item
        (when (and prog (executable-find prog))
          (push implementation valid-impls))))
    (nreverse valid-impls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup slime
;;

(defvar *slime-contribs* '(slime-fancy slime-tramp))
(with-eval-after-load 'slime-company
  (add-to-list '*slime-contribs* 'slime-company t))

(use-package slime-company :ensure t
  :config
  (setopt slime-company-completion 'fuzzy
          slime-company-after-completion 'slime-company-just-one-space
          slime-enable-evaluate-in-emacs t))

(use-package slime :ensure t
  :init
  (when (require 'slime-autoloads nil t)
    (slime-setup *slime-contribs*))
  
  :config
  (setopt slime-lisp-implementations
    (pref.inner/resolve-slime-impls *pref-lisp-impls*)
    lisp-slime-loop-indentation 1
    lisp-loop-keyword-indentation 6
    lisp-loop-forms-indentation 6))

(provide 'pref-slime)
;;; pref-slime.el ends here
