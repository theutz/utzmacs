;;; Utzmacs --- Utz Certified
;;
;; Copyright (C) 2020 Michael Utz
;;
;; Author: Michael Utz <michael@theutz.com>
;; Keywords: convenience
;; URL: https://github.com/theutz/utzmacs
;; Created: 7 Oct 2020

;;; Commentary:
;;
;; This is where I get everthing started.
;;

;;; Code:

(eval-when-compile
  (setq user-emacs-directory "~/.utzmacs")
  (setq-default use-package-expand-minimally t))

(let ((default-directory (expand-file-name "core/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'core)
(require 'use-package)

(use-package evil
  :demand t
  :functions evil-mode
  :init
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-shift-width 2)
  :config
  (evil-mode 1))

(use-package flycheck
  :functions global-flycheck-mode
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package which-key
  :functions which-key-mode
  :config
  (which-key-mode))

(provide 'init)

;;; init.el ends here
