;; Save backup files in ~/.emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; keep windows balanced all the time
(setq window-combination-resize t)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun my/project-files-functions (project)
  "Returns list of files in PROJECT using git. Useful to speed up project.el by only considering version-controlled files"
  (let ((default-directory (project-root project)))
    (when (file-directory-p (expand-file-name ".git" default-directory))
      (process-lines "git" "ls-files" "--cached" "--others" "--exclude-standard"))))

(setq project-files-functions '(my/project-files-functions))

;; set default browser as default OS X browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; helpers
(electric-pair-mode 1)                  ; Auto insert closing bracket
(electric-indent-mode 1)                ; Make return key also do indent, globally
(delete-selection-mode t)               ; Overwrite highlighted text in region
(global-subword-mode 1)                 ; Make cursor movement stop in between camelCase words.
(global-visual-line-mode 1)             ; Make Emacs wrap long lines visually, but not actually
(global-auto-revert-mode 1)             ; When a file is updated outside emacs, make it update if it's already opened in emacs
(delete-selection-mode 1)               ; Overwrite buffer in selected region
(fset 'yes-or-no-p 'y-or-n-p)           ; Treat 'y' or <CR> as yes, 'n' as no.
(setq large-file-warning-threshold nil) ; don't ask for confirmation when opening big files
(setq select-enable-clipboard t)        ; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq select-enable-primary t)          ; after mouse selection in X11, you can paste by `yank' in emacs
(setq sentence-end-double-space nil)
(setq search-whitespace-regexp ".*?")   ; fuzzy-search with isearch
(setq xref-search-program 'ripgrep)
(setq compilation-scroll-output t)
(completion-preview-mode t)
;; (setq compilation-scroll-output 'first-error)

;; abbrevations and completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-styles '(basic initials partial-completion substring)
      completion-category-defaults nil
      completion-show-help nil
      tab-always-indent 'complete)

;; org-mode settings
(setq org-directory "~/.org")
(setq org-agenda-files '("~/.org/tasks.org" "~/.org/agenda.org" "~/.org/thinking.org"))
(setq org-agenda-todo-list-sublevels t)
(setq org-refile-keep t)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets
      '(("~/.org/tasks.org" :maxlevel . 2)
	("~/.org/agenda.org" :maxlevel . 2)
	("~/.org/thinking.org" :maxlevel . 2)))
(setq org-capture-templates
      '(("l" "Quick Thinking" entry
	 (file+headline "~/.org/thinking.org" "Quick Logs")
	 "* Quick Thinking - %U\n** What's the goal?\n%?\n** What's blocking me?\n\n** What's safe to try next?\n- [ ] "
	 :empty-lines 1)))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN_PROGRESS(i)" "|" "DONE(d)" "CANCELED(c)")))

(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (save-some-buffers t (lambda () (member (buffer-file-name) org-agenda-files))))) ;; auto-save refile
