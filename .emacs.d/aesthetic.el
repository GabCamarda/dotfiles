;;; aesthetic.el --- Provides high level aesthetic settings
(set-window-buffer nil (current-buffer)) 		; Use them now.
(set-frame-font "Cascadia Code-12" nil t)               ; Set font and size
(global-hl-line-mode 1)             			; Turn on highlighting current line
(column-number-mode 1)              			; Show cursor position within line
(show-paren-mode t)
(setq-default cursor-type '(hbar . 17))
(set-face-background 'hl-line nil)
(set-face-foreground 'hl-line nil)
(set-face-underline  'hl-line t)
(setq-default left-margin-width 1 right-margin-width 1) ; Define new widths.
(setq show-paren-style 'expression)
(setq inhibit-startup-screen t)     			; Disable startup screen with graphics
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))      ; Disable the menu bar atop screen
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))	; Disable the button bar atop screen
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))	; Disable scroll bar
(setq tab-width 4)                  			; Four spaces is a tab
(setq visible-bell nil)             			; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   			; Disable super annoying audio bell
(setq-default fill-column 80)                           ; M-q should fill at 80 chars, not 75

(setq initial-buffer-choice
  (lambda ()
    (if (buffer-file-name)
      (current-buffer) ;; leave as-is
      (dired-jump))))

(setq simplicity-override-colors-alist
      '(("simplicity-foreground" . "gray80")))

(defun select-next-window () (other-window 1))

(defun load-theme-emacs-client ()
  "Load custom theme on Emacs clients."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (select-frame frame)
		  (load-theme 'simplicity t)))
    (load-theme 'simplicity t)))

(load-theme-emacs-client)
