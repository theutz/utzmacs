;;; Utzmacs --- Utz Certified Emacs
;;
;; Copyright (C) 2020 Michael Utz
;;
;; Author: Michael Utz <michael@theutz.com>
;; Keywords: convenience
;; URL: https://github.com/theutz/utzmacs
;; Created: 7 Oct 2020

;;; Commentary:
;;
;; I wanted to use a literate org config.  So I did.
;;

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))
  ;; Make a file to contain all user customizations
  (setq custom-file
	(expand-file-name "custom.el"
			  (file-name-directory (or load-file-name buffer-file-name))))
  (load custom-file)

  ;; Setup straight.el
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

  ;; Setup org
  (straight-use-package 'org)
  (require 'org)

  ;; Require my literate config
  (org-babel-load-file
   (expand-file-name "utzmacs.org" user-emacs-directory))

  (garbage-collect))

(provide 'init)

;;; init.el ends here
