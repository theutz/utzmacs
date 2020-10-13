;;; Utzmacs --- Utz Certified Emacs
;;
;; Copyright (C) 2020 Michael Utz
;;
;; Author: Michael Utz <michael@theutz.com>
;; Keywords: convenience
;; URL: https://github.com/theutz/utzmacs
;; Created: 7 Oct 2020
;;
;;; Commentary:
;;
;; Let's try this a different way.
;;
;;; Code:

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

(provide 'init)

;;; init.el ends here
