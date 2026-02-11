;;; pref-evil --- Summary: Configurations for evil-mode
;;; Commentary:
;; Movement:
;;   j k i l = left / down / up / right
;;   u / o   = word backward / forward
;;   U / O   = WORD backward / forward
;;
;; Structural motions:
;;   h + x = backward structure
;;   ; + x = forward structure
;;     [ ] = section begin / end
;;     ( ) = paren begin / end
;;     { } = brace begin / end
;;     ' ` = previous / next mark
;;     s   = prev / next flyspell error
;;
;; Insert / Append:
;;   e / E = insert (point / line)
;;   r / R = append (point / line)
;;   w / W = open line below / above
;;
;; Text objects:
;;   e + x = inner
;;   r + x = outer
;;
;; Search / Jump:
;;   q / Q = search word forward / backward
;;   a     = jump to match (%)
;;   , / . = repeat f/t backward / forward
;;
;; Editing:
;;   s = replace char
;;   ! = undo   (C-r = redo)
;;
;; Miscellaneous:
;;   <region>S = evil-surround
;;   b / B     = er/expand-region / er/contract-region
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Core
;;
(declare-function evil-select-search-module "evil-search")

(defun pref.inner/after-evil-mode ()
  "Actions after enter `evil-mode'."
  (electric-pair-local-mode -1))

(use-package evil :ensure t
  :hook ((prog-mode . evil-local-mode)
         (evil-local-mode . pref.inner/after-evil-mode))
  :init
  (setopt evil-want-keybinding nil
          evil-want-integration t
          ;; evil-want-C-d-scroll t
          ;; evil-want-C-u-scroll t
          display-line-numbers-type 'relative
          evil-symbol-word-search t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil On Special Buffers
;;
(defun pref.inner/enable-evil-on-splash ()
  "Enable `evil-motion' on *GNU Emacs* buffer."
  (when (and (string= (buffer-name) "*GNU Emacs*")
             (not evil-local-mode))
    (evil-local-mode 1)
    (evil-motion-state)))

(use-package evil :ensure nil
  :hook (((help-mode apropos-mode text-mode) . evil-local-mode)
         (buffer-list-update . pref.inner/enable-evil-on-splash))
  :config
  ;; `motion-mode' on help buffers
  (evil-set-initial-state 'help-mode 'motion)
  (evil-set-initial-state 'apropos-mode 'motion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third-party
;;
(use-package evil-surround :ensure t
  :after evil
  :hook (evil-local-mode . evil-surround-mode))

(use-package expand-region :ensure t
  :config
  (setopt er/try-expand-list
    '(er/mark-inside-quotes
      er/mark-outside-quotes
      er/mark-inside-pairs
      er/mark-outside-pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;
(defvar pref.general/evil-semicolon-map nil)
(defvar pref.general/evil-h-map nil)

(use-package general :ensure t
  :after evil
  :init
  (require 'general)
  (setf pref.general/evil-semicolon-map (make-sparse-keymap))
  (setf pref.general/evil-h-map (make-sparse-keymap))
  :config
  (general-evil-setup t)

  ;; leader key: \
  (with-eval-after-load 'general
    (general-create-definer pref.evil/leader
      :states '(normal visual motion)
      :keymaps 'override
      :prefix "\\"
      :non-normal-prefix "\\"))

  (pref.evil/leader
   "f" 'find-file
   "b" 'switch-to-buffer)

  ;; SPC / C-SPC instead of C-u / C-d
  (general-define-key
   :states '(normal motion visual)
   "SPC"   #'evil-scroll-down
   "C-SPC" #'evil-scroll-up)

  (general-define-key
   :states '(normal motion visual operator)
   ";" pref.general/evil-semicolon-map)

  (general-define-key
   :keymaps 'pref.general/evil-semicolon-map
   "]" 'evil-forward-section-begin
   "[" 'evil-forward-section-end
   ")" 'evil-next-close-paren
   "}" 'evil-next-close-brace
   "'" 'evil-next-mark-line
   "`" 'evil-next-mark
   "s" 'evil-next-flyspell-error)

  (general-define-key
   :states '(normal motion visual operator)
   "h" pref.general/evil-h-map)

  (general-define-key
   :keymaps 'pref.general/evil-h-map
   "[" 'evil-backward-section-begin
   "]" 'evil-backward-section-end
   "(" 'evil-previous-open-paren
   "{" 'evil-previous-open-brace
   "'" 'evil-previous-mark-line
   "`" 'evil-previous-mark
   "s" 'evil-prev-flyspell-error)

  (general-define-key
   :states '(normal motion visual operator)
   ;; Cursor movement (ijkl layout)
   "j" 'evil-backward-char
   "k" 'evil-next-line
   "i" 'evil-previous-line
   "l" 'evil-forward-char

   ;; word/symbol movement (u: prev / o: next)
   "u" 'evil-backward-word-begin
   "U" 'evil-backward-WORD-begin
   "o" 'evil-forward-word-end
   "O" 'evil-forward-WORD-end

   ;; matching paren(orig. `%')
   "a" 'evil-jump-item
   ;; repeating f/t (,: prev / .: next)
   "." 'evil-repeat-find-char
   )

  (general-define-key
   :states 'normal
   ;; Enter insert mode
   "e" 'evil-insert               ; orig. `i'
   "E" 'evil-insert-line          ; orig. `I'
   "r" 'evil-append               ; orig. 'a'
   "R" 'evil-append-line          ; orig. 'A'
   "w" 'evil-open-below           ; orig. 'o'
   "W" 'evil-open-above           ; orig. 'O'

   ;; Search symbol
   "q" 'evil-ex-search-word-forward  ; orig. `*'
   "Q" 'evil-ex-search-word-backward ; orig. `#'

   ;; Replace character(orig. `r')
   "s" 'evil-replace
   ;; !: undo / C-r: redo
   "!" 'evil-undo
   ;; Record macro: <f3>r ; where r: register alphabet
   "<f3>" 'evil-record-macro      ; orig. `q'
   )

  (general-define-key
   :states 'visual
   "E" 'evil-insert-line
   "R" 'evil-append-line
   "w" 'evil-visual-exchange-corners ; change cursor direction
   )

  (defalias 'evil-inner-text-object-prefix
    evil-inner-text-objects-map)
  (defalias 'evil-outer-text-object-prefix
    evil-outer-text-objects-map)
  ;; text object(v/c/d action): e(inner) / r(outer)
  (general-define-key
   :states '(visual operator)
   "e" 'evil-inner-text-object-prefix
   "r" 'evil-outer-text-object-prefix)

  (general-unbind :states 'normal "S")
  (general-unbind :states 'motion "e")
  (general-define-key
   :states '(motion normal visual)
   "b" nil "B" nil)

  (with-eval-after-load 'expand-region
    (general-define-key
     :states '(normal visual)
     "b" #'er/expand-region
     "B" #'er/contract-region))

  (with-eval-after-load 'ace-window
    (pref.evil/leader
      "o" #'ace-window))
  (with-eval-after-load 'avy
    (general-define-key
     :states '(insert motion normal)
     "C-c ;" 'avy-goto-char-timer)
    (pref.evil/leader
     ";" #'avy-goto-char-timer))
  (with-eval-after-load 'consult
    (pref.evil/leader
     "." #'consult-recent-file
     "b" #'consult-buffer)))

(provide 'pref-evil)
;;; pref-evil.el ends here
