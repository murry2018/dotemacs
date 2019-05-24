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

(defvar *DMZ* t
  "주로 외부 도구의 도움이 필요하여 시스템 설정에 의존하는 경우.
DMZ에서 활성화되는 패키지:
- irony
- flycheck-irony
- company-irony
- company-irony-c-headers")

(defmacro ensure-package (pkg-var)
  "Load package. If the package is not installed, install it."
  `(unless (package-installed-p pkg-var)
     (unless package-archive-contents
       (package-refresh-contents))
     ;; install package
     (package-install pkg-var))
  `(require ,pkg-var))

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

;; 개발용 옵션
(when *DMZ*
  ;; irony : 코드 자동완성, 문법검사 기능을 제공하는 프레임워크
  ;; irony, company-irony, company-irony-c-headers
  ;; GitHub(irony) - https://github.com/Sarcasm/irony-mode
  ;; GitHub(company-~) - https://github.com/Sarcasm/company-irony
  ;; GitHub(~-c-headres) - https://github.com/hotpxl/company-irony-c-headers
  ;; GitHub(flycheck-~) - https://github.com/Sarcasm/flycheck-irony
  (ensure-package 'irony)
  (ensure-package 'company-irony)
  (ensure-package 'company-irony-c-headers)
  (ensure-package 'flycheck-irony)
  (with-eval-after-load "irony"
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    )

  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-irony)
    (add-to-list 'company-backends 'company-irony-c-headers))
  (with-eval-after-load "flycheck"
    (add-hook 'irony-mode-hook 'flycheck-irony-setup)))

(defun init-tasks ()
  (ivy-mode 1)
  (smex-initialize)
  (global-company-mode)
  (global-flycheck-mode)
  (highlight-doxygen-global-mode 1))

(add-hook 'after-init-hook 'init-tasks)
