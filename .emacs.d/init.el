;; Set up package repositories so M-x package-install works.
(require 'package)
(setq package-enable-at-startup nil)
(setq comp-deferred-compilation t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Pull exec-path out of current shell settings, useful for setting the right paths when testing
(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

;; Built-in
(use-package epa-file :init)		   		   ; gpg stuff
(use-package ido                                           ; ido
  :init
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-everywhere 1)
  )

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1)
  )

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :ensure t
  :config
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
	corfu-quit-no-match 'separator) ;; or t
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(require 'grep)
 (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)
 )

(use-package ctags-update :ensure t)
;; (use-package projectile
;;   :ensure
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (setq projectile-enable-caching t)
;;   (setq projectile-indexing-method 'alien)
;;   (setq projectile-globally-ignored-file-suffixes
;;         '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
;;   (setq projectile-globally-ignored-directories
;;         '(".git" "node_modules" "__pycache__" ".vs" "_build"))
;;   (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store" ".settings" ".idea"))
;;   (setq projectile-switch-project-action 'projectile-vc)
;;   (projectile-mode))

;; LSP client with a minimally-intrusive approach
(use-package eglot
  :ensure t
  :defer t
  :bind
  (("C-c r" . 'eglot-rename)
   ("C-c o" . 'eglot-code-action-organize-imports)
   ("C-c h" . 'eldoc))
  )

;; Hacker News client
(use-package hackernews
  :ensure t)

;; Fix trailing spaces but only in modified lines
(use-package ws-butler
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package dumb-jump
  :ensure t
  :bind (("C-c C-g o" . dumb-jump-go-other-window)
         ("C-c C-g j" . dumb-jump-go)
         ("C-c C-g i" . dumb-jump-go-prompt)
         ("C-c C-g x" . dumb-jump-go-prefer-external)
         ("C-c C-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ag)
  )

;; Global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ====== Programming mode setup ======
(load "~/.emacs.d/programming/ocaml_setup.el")
(load "~/.emacs.d/programming/elixir_setup.el")
;; (load "~/.emacs.d/programming/js_setup.el")
;; ====== End programming mode setup ======

;; ====== Custom Settings =====
(load "~/.emacs.d/custom_settings.el")
(load "~/.emacs.d/custom_keybindings.el")
(load "~/.emacs.d/aesthetic.el")
(load "~/.emacs.d/org_mode_setup.el")
(load "~/.emacs.d/erc.el")
;; ====== End Custom Settings =====

;; ====== Emacs custom file =========
(setq custom-file "~/.emacs.d/editor_custom_file.el")
(load custom-file)
;; ====== End Emacs custom file =====
