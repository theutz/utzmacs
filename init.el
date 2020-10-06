;;;; Utzmacs -- My Emacs Setup

;;; Commentary:

;;; Code:

(setq-default straight-fix-flycheck t
	      straight-use-package-by-default t
	      flycheck-emacs-lisp-load-path 'inherit)

(load (concat user-emacs-directory "core/core-straight")
      nil 'nomessage)

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
