;;; configure-packages.el --- Load and configure packages
;;
;;; Commentary:
;;
;;; Code:

(require 'define-utz-namespace)

(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

(use-package general
  :config
  (general-auto-unbind-keys)

  ;; Setup Definer for Leader Key
  (general-create-definer utz/set-leader-key
    :prefix utz/leader-key
    :non-normal-prefix (concat "M-" utz/leader-key)
    :keymaps utz/default-definer-keymaps)

  ;; Setup Definer for Local Leader Key
  (general-create-definer utz/set-localleader-key
    :prefix utz/localleader-key
    :non-normal-prefix (concat "M-" utz/localleader-key))

  ;; Define keys not associated with packages
  (utz/set-leader-key
    "b d" '(kill-this-buffer :wk "Kill Buffer")
    "b n" '(next-buffer :wk "Next Buffer")
    "b p" '(previous-buffer :wk "Previous Buffer")
    "b" '(:ignore t :wk "Buffer")
    "f e R" '(utz/load-init-file :wk "Reload Config File")
    "f e i" '(utz/edit-init-file :wk "Edit Init File")
    "f e r" '(utz/edit-config-file :wk "Edit Config File")
    "f e" '(:ignore t :wk "Emacs")
    "f r" '(revert-buffer :wk "Revert File")
    "f s" '(save-buffer :wk "Save File")
    "f" '(:ignore t :wk "File")
    "g" '(:ignore t :wk "Git")
    "h e" '(emacs-index-search :wk "Search Emacs Manual")
    "h l" '(elisp-index-search :wk "Search Elisp Manual")
    "q" '(:ignore t :wk "Quit")
    "u" '(universal-argument :wk "Universal Argument")
    "w" `(,(general-simulate-key "C-w") :wk "Window"))
  ;; Define Universal Argument Map Keys
  (general-define-key :keymaps 'universal-argument-map
		      (concat utz/leader-key " u") 'universal-argument-more))

(use-package evil
  :demand t
  :custom
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-C-w-in-emacs-state t)
  (evil-shift-width 2)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  :config
  (evil-set-initial-state 'helpful-mode 'motion)
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package flycheck
  :functions global-flycheck-mode
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :general
  ("C-SPC" '(company-complete :wk "Company Complete")))

(use-package which-key
  :demand t
  :functions which-key-mode
  :general
  (utz/set-leader-key :infix "h"
    "K" '(which-key-show-top-level :wk "Which Key Show Top Level")
    "M" '(which-key-show-major-mode :wk "Which Key Show Major Mode"))
  :custom
  (which-key-idle-delay 0.3)
  (which-key-max-description-length 40)
  (which-key-add-column-padding 1)
  :config
  (which-key-mode))

(use-package restart-emacs
  :general
  (utz/set-leader-key
    "q r" '(restart-emacs :wk "Restart Emacs")))

(use-package magit
  :general
  (utz/set-leader-key :infix "g"
    "SPC" '(magit-status :wk "Magit Status")
    "RET" '(magit-dispatch :wk "Magit Dispatch")
    "s" '(magit-stage-file :wk "Magit Stage File")))

(use-package evil-magit
  :after (evil magit))

(use-package org
  :straight org-plus-contrib
  :general
  (utz/set-leader-key :infix "o"
    "a SPC" '(org-agenda :wk "Org Agenda")
    "a" '(:ignore t :wk "Agenda"))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-agenda-include-diary t)
  (org-directory "~/org")
  (diary-file (expand-file-name "diary" org-directory))
  :init
  ;; `:init' here is ineffective, since org is loaded
  ;; when tangling this file. To execute code before `org-mode'
  ;; is loaded, use `init.el';
  :config)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-mac-iCal
  :straight (org-mac-iCal :type git :host github :repo "ndw/org-mac-iCal"
			  :fork (:host github
				       :repo "theutz/org-mac-iCal"))
  :custom
  (org-mac-iCal-calendar-names '("Personal")))

(use-package helpful
  :general
  (utz/set-leader-key :infix "h"
    "SPC" '(helpful-at-point :wk "Helpful At Point")
    "C" '(helpful-command :wk "Helpful Command")
    "F" '(helpful-function :wk "Helpful Function")
    "f" '(helpful-callable :wk "Helpful Callable")
    "k" '(helpful-key :wk "Helpful Key")
    "v" '(helpful-variable :wk "Helpful Variable")))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-outrun-electric t)
  (doom-themes-visual-bell-config)
  ;;(doom-themes-neotree-config)
  ;;(setq doom-themes-treemacs-theme "doom-colors")
  ;;(doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package counsel
  :custom-face
  (ivy-current-match ((t (:inherit 'default
				   :background ,(face-attribute 'default :foreground)
				   :foreground ,(face-attribute 'default :background)))))
  :general
  ("C-s" '(swiper-isearch :wk "Search")
   "C-x C-b" '(ivy-switch-buffer :wk "Switch Buffer"))
  (utz/set-leader-key
    "/" '(swiper-isearch :wk "Search")
    "SPC" '(counsel-M-x :wk "M-x")
    "b b" '(ivy-switch-buffer :wk "List Buffers")
    "f /" '(swiper-isearch :wk "Search in File")
    "f f" '(counsel-find-file :wk "Find File")
    "h ," '(counsel-describe-face :wk "Describe Face"))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package hydra)

(use-package ivy-hydra)

(provide 'configure-packages)

;;; configure-packages.el ends here
