;;; configure-emacs.el --- Tweaks that aren't package-related.
;;
;;; Commentary:
;;
;;; Code:

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(tool-bar-mode 0)

(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-type 'visual)

(show-paren-mode 1)

(menu-bar-mode 0)

(scroll-bar-mode 0)

(setq inhibit-startup-screen t
      initial-scratch-message nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq enable-local-variables :safe)

(custom-set-faces `(default
		     ((t (:family ,(face-attribute 'utz/fixed-width-font :family)
				  :height 120)))))

(provide 'configure-emacs)

;;; configure-emacs.el ends here
