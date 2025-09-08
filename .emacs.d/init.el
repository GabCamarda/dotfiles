;; Set up package repositories so M-x package-install works.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq desktop-path '("~/.emacs.d/" "~" "."))
(fido-mode t)
(fido-vertical-mode t)

;; Packages
(use-package simplicity-theme :ensure t)
(use-package deadgrep :ensure t :defer t)
(use-package change-inner :ensure t :defer t)       ; Similar to ci or co in Vim
(use-package dumb-jump
  :ensure t
  :defer t
  :config
  (setq dumb-jump-force-searcher 'ag
	dumb-jump-prefer-tooltip nil
	dumb-jump-debug nil)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Custom settings
(load-file "~/.emacs.d/custom_settings.el")
;; High level aesthetic stuff
(load-file "~/.emacs.d/aesthetic.el")
;; Make keyboard bindings not suck
(load-file "~/.emacs.d/custom_keybindings.el")

;; ====== Programming mode setup ======
;; (require 'elixir_setup) -> ~/.emacs.d/programming_setup/elixir_setup.el
;; ensure each setup is only loaded once, but only for a file with the relevant file extension
(add-to-list 'load-path "~/.emacs.d/programming_setup")
;; (require 'elixir_setup)
(add-to-list 'auto-mode-alist '("\\.exs?\\'" . (lambda () (require 'elixir_setup))))
(add-to-list 'auto-mode-alist '("\\.mli?\\|\\.mll\\|\\.mly\\'" . (lambda () (require 'ocaml_setup))))
;; ====== End programming mode setup ======

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6843739510a2707c5b2822427bf11d46e952f09d8df8589ff577f5e3c4efd31a"
     "3aa51468052c1e3e21dd41a3fa40c0161e07ca600683e3d96f1bca70f36749e2"
     "8899e88d19a37d39c7187f4bcb5bb596fba990728ef963420b93e2aea5d1666a" default))
 '(package-selected-packages '(change-inner deadgrep dumb-jump exunit simplicity-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(font-lock-comment-face ((t (:foreground "brightblack"))))
 '(font-lock-function-name-face ((t (:foreground "#ffffff" :weight bold))))
 '(show-paren-match ((t (:background "mistyrose4" :foreground "gray85" :underline "mistyrose4" :weight bold)))))
