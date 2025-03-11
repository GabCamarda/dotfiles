;;; aesthetic.el --- Provides high level aesthetic settings
(set-window-buffer nil (current-buffer)) 		; Use them now.
(set-frame-font "Cascadia Code-12" nil t)               ; Set font and size
(global-hl-line-mode 1)             			; Turn on highlighting current line
(column-number-mode 1)              			; Show cursor position within line
(show-paren-mode t)
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

;; note, if the fonts are weird, run `M-x all-the-icons-install-fonts`
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;; colorize the output of the compilation mode.
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))

;;   ;; mocha seems to output some non-standard control characters that
;;   ;; aren't recognized by ansi-color-apply-on-region, so we'll
;;   ;; manually convert these into the newlines they should be.
;;   (goto-char (point-min))
;;   (while (re-search-forward "\\[2K\\[0G" nil t)
;;     (progn
;;       (replace-match "
;; ")))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq initial-buffer-choice
  (lambda ()
    (if (buffer-file-name)
      (current-buffer) ;; leave as-is
      (dired-jump))))

(defun select-next-window () (other-window 1))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window
    				 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Zoom text when needed
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))
(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))

(defun load-theme-emacs-client ()
  "Load custom theme on Emacs clients."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (select-frame frame)
		  (load-theme 'sanityinc-tomorrow-night t)))
    (load-theme 'sanityinc-tomorrow-night t)))

(require 'color-theme-sanityinc-tomorrow)
(load-theme-emacs-client)

;;; aesthetic.el ends here
