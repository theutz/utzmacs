;; General

(straight-use-package 'general)
(require 'general)
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
  "w" '(evil-window-map :wk "Window"))

;; Define Universal Argument Map Keys
(general-define-key :keymaps 'universal-argument-map
		    (concat utz/leader-key " u") 'universal-argument-more)



(utz/set-leader-key "p" 'projectile-command-map)

(utz/set-leader-key :infix "h"
  "K" '(which-key-show-top-level :wk "Which Key Show Top Level")
  "M" '(which-key-show-major-mode :wk "Which Key Show Major Mode"))

(utz/set-leader-key "q r" '(restart-emacs :wk "Restart Emacs"))

(utz/set-leader-key :infix "g"
  "SPC" '(magit-status :wk "Magit Status")
  "RET" '(magit-dispatch :wk "Magit Dispatch")
  "s" '(magit-stage-file :wk "Magit Stage File"))

(utz/set-leader-key :infix "o"
  "a SPC" '(org-agenda :wk "Org Agenda")
  "a" '(:ignore t :wk "Agenda"))

(utz/set-leader-key :infix "h"
  "SPC" '(helpful-at-point :wk "Helpful At Point")
  "C" '(helpful-command :wk "Helpful Command")
  "F" '(helpful-function :wk "Helpful Function")
  "f" '(helpful-callable :wk "Helpful Callable")
  "k" '(helpful-key :wk "Helpful Key")
  "v" '(helpful-variable :wk "Helpful Variable"))

(general-define-key
 "C-s" '(swiper-isearch :wk "Search")
 "C-x C-b" '(ivy-switch-buffer :wk "Switch Buffer"))

(utz/set-leader-key
  "/" '(swiper-isearch :wk "Search")
  "SPC" '(counsel-M-x :wk "M-x")
  "b b" '(ivy-switch-buffer :wk "List Buffers")
  "f /" '(swiper-isearch :wk "Search in File")
  "f f" '(counsel-find-file :wk "Find File")
  "h ," '(counsel-describe-face :wk "Describe Face"))



(provide 'utz-keybindings)
