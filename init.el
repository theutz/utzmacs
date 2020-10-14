(let ((gc-cons-threshold most-positive-fixnum)
      (current-directory (file-name-directory (or load-file-name buffer-file-name))))

  ;; Make a file to contain all user customizations
  (setq custom-file
	(expand-file-name "custom.el" current-directory))
  (load custom-file)

  ;; Setup load path
  (add-to-list 'load-path
	       (expand-file-name "lisp/" current-directory))

  ;; Main loading sequence
  (require 'setup-package-tools)
  (require 'define-utz-namespace)
  (require 'configure-emacs)
  (require 'configure-packages))
