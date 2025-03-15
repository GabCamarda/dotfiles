;; Save backup files in ~/.emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; keep windows balanced all the time
(setf window-combination-resize t)

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
;; (setq compilation-scroll-output 'first-error)
