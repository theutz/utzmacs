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

  ;; Setup straight.el
  (defvar bootstrap-version)
  (setq straight-fix-flycheck t)
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

  ;; Setup use-package
  (straight-use-package 'use-package)

  (load (expand-file-name "utzmacs.el" current-directory))

  (garbage-collect))

(provide 'init)

;;; init.el ends here
