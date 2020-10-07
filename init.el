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

;; Setup Straight

(setq-default straight-fix-flycheck t
	      straight-use-package-by-default t)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; Setup Use Package

(straight-use-package 'use-package)
(require 'use-package)

;; Setup Evil

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

;; Setup Which Key

(use-package which-key
  :functions which-key-mode
  :config
  (which-key-mode))

;; Provide the Feature

(provide 'init)

;;; init.el ends here
