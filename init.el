;;; Utzmacs --- Utz Certified
;;;
;;; Author: Michael Utz <michael@theutz.com>
;;;
;;; Commentary:
;;;
;;; This is where I get everthing started.
;;;
;;; Code:

(let ((default-directory (expand-file-name "core/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'core)
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
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(provide 'init)

;;; init.el ends here
