;;; early-init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setopt gc-cons-threshold 100000000) ; ~100 MB
(setq gc-cons-percentage 0.6)

(provide 'early-init)
;;; early-init.el ends here
