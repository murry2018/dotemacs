;;; pref-treesit --- Configurations for treesit.el
;;; Author: JY Lee
;;; Commentary:
;;
;; Grammars built with a different version of the system's `libtree-sitter'
;; library will not be loaded by Emacs. Since it is undesirable to add such
;; unavailable grammars to `major-mode-remap-alist', this code only includes
;; languages that Emacs reports as available via
;; `treesit-language-available-p'.
;;
;; (See the use-package configuration for 'treesit' for related setup.)
;;
;;; Code:
(defvar pref.inner/*treesit-remap-table*
  '((:lang awk :mode awk-mode :remap-mode awk-ts-mode)
    (:lang bash :mode sh-mode :remap-mode bash-ts-mode)
    (:lang bibtex :mode bibtex-mode :remap-mode bibtex-ts-mode)
    (:lang blueprint :mode blueprint-mode :remap-mode blueprint-ts-mode)
    (:lang c :mode c-mode :remap-mode c-ts-mode)
    (:lang c-sharp :mode csharp-mode :remap-mode csharp-ts-mode)
    (:lang clojure :mode (clojure-mode clojurescript-mode clojurec-mode)
           :remap-mode clojure-ts-mode)
    (:lang cmake :mode cmake-mode :remap-mode cmake-ts-mode)
    (:lang commonlisp :mode common-lisp-mode :remap-mode commonlisp-ts-mode)
    (:lang cpp :mode c++-mode :remap-mode c++-ts-mode)
    (:lang css :mode css-mode :remap-mode css-ts-mode)
    (:lang dart :mode dart-mode :remap-mode dart-ts-mode)
    (:lang dockerfile :mode dockerfile-mode :remap-mode dockerfile-ts-mode)
    (:lang elixir :mode elixir-mode :remap-mode elixir-ts-mode)
    (:lang glsl :mode glsl-mode :remap-mode glsl-ts-mode)
    (:lang go :mode go-mode :remap-mode go-ts-mode)
    (:lang gomod :mode go-mod-mode :remap-mode go-mod-ts-mode)
    (:lang heex :mode heex-mode :remap-mode heex-ts-mode)
    (:lang html :mode (mhtml-mode sgml-mode) :remap-mode html-ts-mode)
    (:lang janet :mode janet-mode :remap-mode janet-ts-mode)
    (:lang java :mode java-mode :remap-mode java-ts-mode)
    (:lang javascript :mode (js-mode javascript-mode js2-mode) :remap-mode
           js-ts-mode)
    (:lang json :mode js-json-mode :remap-mode json-ts-mode)
    (:lang julia :mode julia-mode :remap-mode julia-ts-mode)
    (:lang kotlin :mode kotlin-mode :remap-mode kotlin-ts-mode)
    (:lang latex :mode latex-mode :remap-mode latex-ts-mode)
    (:lang lua :mode lua-mode :remap-mode lua-ts-mode)
    (:lang magik :mode magik-mode :remap-mode magik-ts-mode)
    (:lang make :mode makefile-mode :remap-mode makefile-ts-mode)
    (:lang markdown :mode (poly-markdown-mode markdown-mode) :remap-mode
           markdown-ts-mode)
    (:lang nix :mode nix-mode :remap-mode nix-ts-mode)
    (:lang nu :mode nushell-mode :remap-mode nushell-ts-mode)
    (:lang org :mode org-mode :remap-mode org-ts-mode)
    (:lang perl :mode perl-mode :remap-mode perl-ts-mode)
    (:lang proto :mode protobuf-mode :remap-mode protobuf-ts-mode)
    (:lang python :mode python-mode :remap-mode python-ts-mode)
    (:lang r :mode ess-mode :remap-mode r-ts-mode)
    (:lang ruby :mode ruby-mode :remap-mode ruby-ts-mode)
    (:lang rust :mode rust-mode :remap-mode rust-ts-mode)
    (:lang scala :mode scala-mode :remap-mode scala-ts-mode)
    (:lang sql :mode sql-mode :remap-mode sql-ts-mode)
    (:lang surface :mode surface-mode :remap-mode surface-ts-mode)
    (:lang toml :mode (conf-toml-mode toml-mode) :remap-mode toml-ts-mode)
    (:lang tsx :mode (typescript-tsx-mode) :remap-mode tsx-ts-mode)
    (:lang typescript :mode typescript-mode :remap-mode typescript-ts-mode)
    (:lang typst :mode typst-mode :remap-mode typst-ts-mode)
    (:lang verilog :mode verilog-mode :remap-mode verilog-ts-mode)
    (:lang vhdl :mode vhdl-mode :remap-mode vhdl-ts-mode)
    (:lang vue :mode vue-mode :remap-mode vue-ts-mode)
    (:lang wast :mode wat-mode :remap-mode wat-ts-wast-mode)
    (:lang wat :mode wat-mode :remap-mode wat-ts-mode)
    (:lang wgsl :mode wgsl-mode :remap-mode wgsl-ts-mode)
    (:lang yaml :mode yaml-mode :remap-mode yaml-ts-mode))

  "A list of triplets used to construct a `major-mode-remap-alist'.

LANG is the language symbol.
MODE is a major mode symbol, or a list of major mode symbols.
REMAP-MODE is the corresponding tree-sitter major mode symbol.

This list is indebted to the original `treesit-auto-mode' source code by
@renzmann.")

(defvar pref.inner/*treesit-available-languages*)

(cl-defun pref.inner/normalize-available-remap-list (&key lang mode remap-mode)
  "Normalize mode, and make a list of (MODE . REMAP-MODE).
With LANG `treesit-language-available-p',
If mode is a symbol, immediately returns ((MODE . REMAP-MODE)).
If mode is a list, it makes a list such as:
  ((MODE1 . REMAP-MODE)
   (MODE2 . REMAP-MODE)
   (MODE3 . REMAP-MODE)) and so on.
But with LANG not `treesit-language-available-p', it returns nil."

  (when (treesit-language-available-p lang)
    (if (listp mode)
        (mapcar #'(lambda (each-mode) (cons each-mode remap-mode))
                mode)
      `((,mode . ,remap-mode)))))

(use-package treesit
  :init
  (setf pref.inner/*treesit-available-languages*
        (cl-loop for ent in pref.inner/*treesit-remap-table*
                 nconc (apply #'pref.inner/normalize-available-remap-list ent)))
  (setopt major-mode-remap-alist pref.inner/*treesit-available-languages*))

(use-package treesit-fold :ensure t
  :init
  (if (fboundp 'global-treesit-fold-indicators-mode)
      (global-treesit-fold-indicators-mode)
    (global-treesit-fold-mode))
  :config
  (setopt treesit-fold-line-count-show t
          treesit-fold-line-count-format " <%d lines> "))

(provide 'pref-treesit)
;;; pref-treesit.el ends here
