;;; core-straight.el --- Setup Straight  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar bootstrap-version)

(defun utzmacs--bootstrap-straight ()
  "Download straight.el and load it."
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
    (load bootstrap-file nil 'nomessage)))

(utzmacs--bootstrap-straight)

(provide 'core-straight)

;;; core-straight.el ends here
