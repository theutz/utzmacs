;; Garbage Collection Magic Hook

(straight-use-package 'gcmh)
(require 'gcmh)
(gcmh-mode 1)



;; Evil

(straight-use-package 'evil)
(setq-default evil-split-window-below t
	      evil-vsplit-window-right t
	      evil-want-C-u-scroll t
	      evil-want-C-u-delete t
	      evil-want-C-w-in-emacs-state t
	      evil-shift-width 2
	      evil-want-keybinding nil
	      evil-want-integration t)
(require 'evil)
(with-eval-after-load 'evil
  (evil-set-initial-state 'helpful-mode 'motion)
  (evil-mode 1))

;; Evil Surround

(straight-use-package 'evil-surround)
(with-eval-after-load 'evil
  (require 'evil-surround)
  (with-eval-after-load 'evil-surround
    (when (fboundp 'global-evil-surround-mode)
      (global-evil-surround-mode 1))))

;; Evil Collection

(straight-use-package 'evil-collection)
(with-eval-after-load 'evil
  (require 'evil-collection)
  (with-eval-after-load 'evil-collection
    (when (fboundp 'evil-collection-init)
      (evil-collection-init))))



;; Flycheck

(straight-use-package 'flycheck)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(require 'flycheck)
(with-eval-after-load 'flycheck
  (global-flycheck-mode))



;; Company

(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'general
  (general-define-key "C-SPC" '(company-complete :wk "Company Complete")))



;; Which Key

(straight-use-package 'which-key)
(setq-default which-key-idle-delay 0.3
	      which-key-max-description-length 40
	      which-key-add-column-padding 1)
(require 'which-key)
(which-key-mode)



;; Projectile

(straight-use-package 'projectile)
(require 'projectile)
(projectile-mode +1)



;; Restart Emacs

(straight-use-package 'restart-emacs)
(require 'restart-emacs)



;; Magit

(straight-use-package 'magit)
(require 'magit)

;; Evil Magit

(straight-use-package 'evil-magit)
(with-eval-after-load 'evil
  (with-eval-after-load 'magit
    (require 'evil-magit)))



;; Org

(straight-use-package 'org-plus-contrib)
(require 'org)
(setq org-confirm-babel-evaluate nil
      org-directory "~/org"
      diary-file (expand-file-name "diary" org-directory))
(setq-default org-agenda-include-diary t)

;; Org Bullets

(straight-use-package 'org-bullets)
(require 'org-bullets)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'org-bullets-mode))



;; Helpful

(straight-use-package 'helpful)
(require 'helpful)



;; Doom Themes

(straight-use-package 'doom-themes)
(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-outrun-electric t)
(doom-themes-visual-bell-config)
;;(doom-themes-neotree-config)
;;(setq doom-themes-treemacs-theme "doom-colors")
;;(doom-themes-treemacs-config)
(doom-themes-org-config)



;; YASnippet

(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)



;; WS Butler

(straight-use-package 'ws-butler)
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)



;; Counsel / Ivy / Swiper

(straight-use-package 'counsel)
(require 'counsel)
(custom-set-faces `(ivy-current-match ((t (:inherit 'default
				   :background ,(face-attribute 'default :foreground)
				   :foreground ,(face-attribute 'default :background))))))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(ivy-mode 1)



;; Hydra

(straight-use-package 'hydra)
(require 'hydra)

(straight-use-package 'ivy-hydra)
(with-eval-after-load 'ivy
  (with-eval-after-load 'hydra
    (require 'ivy-hydra)))



(provide 'configure-packages)
