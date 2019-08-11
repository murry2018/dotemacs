;; package archive settings
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http://" "https://")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
                 (cons "gnu" (concat proto "elpa.gnu.org/packages/")))))
(package-initialize)

;; Packages requires external utilities
(defvar *slime-switch* nil
  "slime, ac-slime
- LISP repl is required.")
(defvar *irony-switch* nil
  "irony, flycheck-irony, company-irony, company-irony-c-headers
- cmake and libclang are required.")

(defmacro ensure-package (pkg-var)
  "pkg-var을 로딩한다. 없다면 알아서 설치한다.
예제: (ensure-package 'ivy)"
 `(progn
    (unless (package-installed-p ,pkg-var)
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install ,pkg-var))
    (require ,pkg-var)))

;; ivy : 미니버퍼 tab-completion을 꾸며준다. 
;; Manual - http://oremacs.com/swiper/
;; GitHub - https://github.com/abo-abo/swiper
(ensure-package 'ivy)
(with-eval-after-load "ivy"
  (setq-default ivy-use-virtual-buffers t)
  (setq-default ivy-count-format "(%d/%d)"))

;; smex : 자주 쓰는 명령어를 기반으로 명령어 추천을 제공한다.
;; GitHub - https://github.com/nonsequitur/smex
(ensure-package 'smex)
(with-eval-after-load "smex"
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; company : 코드 자동완성 프레임워크. 기본적인 자동완성을 제공함
;; Official - https://company-mode.github.io/
;; GitHub - https://github.com/company-mode/company-mode
(ensure-package 'company)
(with-eval-after-load "company"
  ;; I will use ac-slime for lisp-mode
  (add-hook 'slime-mode-hook (lambda () (company-mode -1)))
  (add-hook 'slime-repl-mode-hook (lambda () (company-mode -1))))

;; flycheck : 코드 구문을 검사해준다.
;; Official - https://www.flycheck.org/en/latest/
(ensure-package 'flycheck)
(with-eval-after-load "flycheck"
  (add-hook 'emacs-lisp-mode 'flycheck-mode)
  (add-hook 'c++-mode 'flycheck-mode)
  (add-hook 'c-mode 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; highlight-doxygen : Doxygen 주석의 코드 강조
;; GitHub - https://github.com/Lindydancer/highlight-doxygen
(ensure-package 'highlight-doxygen)

;; Nov.el : Major mode for reading EPUBs
;; Github : https://github.com/wasamasa/nov.el
(ensure-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun nov-tasks ()
  (local-set-key (kbd "C-j") 'nov-next-document)
  (local-set-key (kbd "C-k") 'nov-previous-document))
(add-hook 'nov-mode-hook 'nov-tasks)

(when *slime-switch*
    ;; SLIME : The Superior Lisp Interaction Mode for Emacs
  ;; Official : https://common-lisp.net/project/slime/
  (ensure-package 'slime)
  (with-eval-after-load "slime"
    (setq inferior-lisp-program "sbcl")
    (slime-setup '(slime-fancy)))
  
  ;; ac-slime : Slime completion source for auto-complete package
  ;; GitHub : https://github.com/purcell/ac-slime
  (ensure-package 'ac-slime)
  (with-eval-after-load "ac-slime"
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    ;; Note: In my config, auto-complete is used only for slime
    ;; Note: Installing ac-slime, package.el automatically installs
    ;;       auto-complete as a dependency.
    (with-eval-after-load "auto-complete"
      (define-key ac-completing-map (kbd "RET") 'ac-complete)
      (add-hook 'slime-mode-hook 'auto-complete-mode)
      (add-hook 'slime-repl-mode-hook 'auto-complete-mode)
      (add-to-list 'ac-modes 'slime-repl-mode))))

(when *irony-switch*
  ;; irony : 코드 자동완성, 문법검사 기능을 제공하는 프레임워크
  ;; irony, company-irony, company-irony-c-headers
  ;; GitHub(irony) - https://github.com/Sarcasm/irony-mode
  (ensure-package 'irony)
  (with-eval-after-load "irony"
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    
    ;; GitHub - https://github.com/Sarcasm/company-irony
    (ensure-package 'company-irony)
    ;; GitHub - https://github.com/hotpxl/company-irony-c-headers
    (ensure-package 'company-irony-c-headers)
    (with-eval-after-load "company"
      (add-to-list 'company-backends 'company-irony)
      (add-to-list 'company-backends 'company-irony-c-headers))

    ;; GitHub - https://github.com/Sarcasm/flycheck-irony
    (ensure-package 'flycheck-irony)
    (with-eval-after-load "flycheck"
      (add-hook 'irony-mode-hook 'flycheck-irony-setup))))

(defun init-tasks ()
  (ivy-mode 1)
  (smex-initialize)
  (global-company-mode)
  (global-flycheck-mode)
  (highlight-doxygen-global-mode 1))

(add-hook 'after-init-hook 'init-tasks)
