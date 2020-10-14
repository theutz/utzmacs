;;; setup-package-tools.el --- Setup straight.el
;;
;;; Commentary:
;;
;;; Code:

(setq-default straight-fix-flycheck t
	      straight-use-package-by-default t)

;; Initialize Straight
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

;; Load use-package
(when (fboundp 'straight-use-package)
  (straight-use-package 'use-package))

(provide 'setup-package-tools)
(require 'setup-package-tools)

;;; setup-package-tools.el ends here
