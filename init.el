;;;; Utzmacs -- My Emacs Setup

;;; Commentary:

;;; Code:

(setq-default straight-fix-flycheck t
	      straight-use-package-by-default t
	      flycheck-emacs-lisp-load-path 'inherit)

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
(straight-use-package 'use-package)
(require 'use-package)

(use-package evil
  :functions (evil-mode)
  :config
  (evil-mode))

(use-package flycheck
  :functions (global-flycheck-mode)
  :ensure t
  :init
  (global-flycheck-mode))

(provide 'init)

;;; init.el ends here
