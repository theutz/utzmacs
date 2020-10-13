;;; define-utz-namespace.el --- Define the utz namespace
;;
;;; Commentary:
;;
;; This is all I've made.
;;
;;; Code:

(defgroup utz nil
  "All my customization options."
  :group 'convenience)

(defcustom utz/leader-key "SPC"
  "Like VIM's leader."
  :type 'string
  :initialize 'custom-initialize-default
  :group 'utz)

(defcustom utz/localleader-key ","
  "Like VIM's localleader."
  :type 'string
  :initialize 'custom-initialize-default
  :group 'utz)

(defcustom utz/default-definer-keymaps '(normal insert visual emacs)
  "The default evil modes used in my custom definer."
  :type '(repeat symbol)
  :initialize 'custom-initialize-default
  :group 'utz)

(defcustom utz/init-file (expand-file-name "init.el" user-emacs-directory)
  "File where Utzmacs is bootstrapped."
  :type 'file
  :initialize 'custom-initialize-default
  :group 'utz)

(defcustom utz/config-file (expand-file-name "utzmacs.org" user-emacs-directory)
  "This file."
  :type 'file
  :initialize 'custom-initialize-default
  :group 'utz)

(defcustom utz/custom-file (cond ((boundp 'custom-file) custom-file)
				 (t (expand-file-name "custom.el" user-emacs-directory)))
  "Where to store customizations."
  :type 'file
  :initialize 'custom-initialize-default
  :group 'utz)

(defun utz/load-config-file ()
  "(Re)load the configuration file."
  (interactive)
  (load utz/init-file nil 'nomessage))

(defun utz/edit-init-file ()
  "Edit the init file.

By default, this command opens your config file
in another window.

If called interactively, press `C-u' once ton
open it in the same window."
  (interactive)
  (let ((opener (pcase current-prefix-arg
		  ('(4) 'find-file)
		  (- 'find-file-other-window))))
    (funcall opener utz/init-file)))

(defun utz/edit-config-file ()
  "Edit this configuration file.

By default, this command opens your config
file in another window.

If called interactively, press `C-u' once to
open it in the same window."
  (interactive)
  (let ((opener (pcase current-prefix-arg
		  ('(4) 'find-file)
		  (- 'find-file-other-window))))
    (funcall opener utz/config-file)))

(defgroup utz/faces nil
  "All my face customizations."
  :group 'faces)

(defface utz/display-font '((t (:inherit 'default :family "SF Pro")))
  "Face for titles, headings and other non-fixed-width applications."
  :group 'utz/faces)

(defface utz/fixed-width-font '((t (:inherit 'default :family "BlexMono Nerd Font")))
  "Face for code and other fixed-width applications."
  :group 'utz/faces)

(provide 'define-utz-namespace)

;;; define-utz-namespace.el ends here
