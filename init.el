(let ((gc-cons-threshold most-positive-fixnum))

  ;; Make a file to contain all user customizations
  (setq custom-file
	(expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)

  ;; Setup load path
  (add-to-list 'load-path
	       (expand-file-name "lisp/" user-emacs-directory))

  ;; Main loading sequence
  (require 'setup-package-tools)
  (require 'define-utz-namespace)
  (require 'configure-emacs)
  (require 'configure-packages)
  (require 'utz-keybindings))
