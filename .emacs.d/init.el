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

(use-package use-package-ensure-system-package :ensure t)
;; Pull exec-path out of current shell settings, useful for setting the right paths when testing
(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

;; 3rd party
(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/snippets")
(use-package epa-file)		   		   ; gpg stuff, built-in

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package magit :ensure t :defer t)
(use-package flycheck :ensure t :defer t)
(use-package yasnippet :ensure t :defer t)
(use-package yasnippet-snippets :ensure t :defer t)
(use-package hydra :ensure t :defer t)
(use-package counsel-dash :ensure t :defer t)
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers nil)
  (company-tooltip-align-annotations 't)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-code-ignore-case nil)
  (company-dabbrev-downcase nil)
  (global-company-mode t)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))
(use-package company-go
  :ensure t
  :config (add-to-list 'company-backends 'company-go))

(use-package projectile
  :ensure
  :config
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store" ".settings" ".idea"))
  (setq projectile-switch-project-action 'projectile-vc)
  (projectile-mode))

;; LSP client with a minimally-intrusive approach
(use-package eglot
  :ensure t
  :defer t
  :bind
  (("C-c r" . 'eglot-rename)
   ("C-c o" . 'eglot-code-action-organize-imports)
   ("C-c h" . 'eldoc))
  :config
  (setq-default eglot-workspace-configuration
		'((:gopls .
			  ((staticcheck . t)
			   (matcher . "CaseSensitive")))))
  )

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init (ivy-mode 1) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil)
  )

;; Enrich buffers
(use-package ivy-rich
  :ensure t
  :after ivy
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich--ivy-switch-buffer-transformer))

(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;; Visual summary of current file
(use-package popup-imenu
  :ensure t
  :bind ("C-x C-i" . popup-imenu))

;; Stack overflow client
(use-package sx
  :ensure t
  :init
  :config
  (bind-keys :prefix "C-c C-s s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

;; Hacker News client
(use-package hackernews
  :ensure t)

;; Fix trailing spaces but only in modified lines
(use-package ws-butler
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package hideshow
  :ensure t
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

(use-package dumb-jump
  :ensure t
  :bind (("C-c C-g o" . dumb-jump-go-other-window)
         ("C-c C-g j" . dumb-jump-go)
         ("C-c C-g i" . dumb-jump-go-prompt)
         ("C-c C-g x" . dumb-jump-go-prefer-external)
         ("C-c C-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)
  :ensure)

;; Global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'xref-backend-definitions #'dumb-jump-xref-activate)

;; Custom settings
(load "~/.emacs.d/custom_settings.el")
;; High level aesthetic stuff
(load "~/.emacs.d/aesthetic.el")
;; Make keyboard bindings not suck
(load "~/.emacs.d/custom_keybindings.el")

;; ====== Programming mode setup ======
(load "~/.emacs.d/racket_setup.el")
(load "~/.emacs.d/c_setup.el")
(load "~/.emacs.d/go_setup.el")
(load "~/.emacs.d/scala_setup.el")
(load "~/.emacs.d/org_mode_setup.el")
(load "~/.emacs.d/ocaml_setup.el")
(load "~/.emacs.d/elixir_setup.el")
(load "~/.emacs.d/js_setup.el")
(load "~/.emacs.d/python_setup.el")
;; ====== End programming mode setup ======

;; ====== Custom Settings =====
(load "~/.emacs.d/erc.el")
(load "~/.emacs.d/calendar.el")
;; ====== End Custom Settings =====

;; ====== Emacs custom file =========
(setq custom-file "~/.emacs.d/editor_custom_file.el")
(load custom-file)
;; ====== End Emacs custom file =====
