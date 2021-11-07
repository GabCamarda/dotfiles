;;; custom_settings.el --- A list of custom settings
;;; Commentary:
;;; Code:

;; Save backup files in ~/.emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Settings to send emails with Gmail
(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 user-mail-address "gabrycamarda@gmail.com"
 smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
 smtpmail-auth-credentials  (expand-file-name "~/.authinfo")
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-debug-info t
 starttls-extra-arguments nil
 starttls-gnutls-program "/usr/bin/gnutls-cli"
 starttls-extra-arguments nil
 starttls-use-gnutls t
 )

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

(defun magit/visit-pr-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v" #'magit/visit-pr-url))

;; Set default browser as default OS X browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(yas-global-mode 1)
;(add-hook 'after-init-hook #'global-flycheck-mode) ; As-you-type error highlighting

;; Also works for Java.
;; (autoload 'google-set-c-style "google-c-style")
;; (autoload 'google-make-newline-indent "google-c-style")
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; helpers
(electric-pair-mode 1)      	   ; Auto insert closing bracket
(electric-indent-mode 1)    	   ; Make return key also do indent, globally
(delete-selection-mode t)	   ; Overwrite highlighted text in region
(global-subword-mode 1)	     	   ; Make cursor movement stop in between camelCase words.
(global-visual-line-mode 1)	   ; Make Emacs wrap long lines visually, but not actually
(global-auto-revert-mode 1) 	   ; When a file is updated outside emacs, make it update if it's already opened in emacs
(delete-selection-mode 1)   	   ; Overwrite buffer in selected region
(fset 'yes-or-no-p 'y-or-n-p)	   ; Treat 'y' or <CR> as yes, 'n' as no.
(setq select-enable-clipboard t)   ; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq select-enable-primary t)     ; after mouse selection in X11, you can paste by `yank' in emacs
(setq sentence-end-double-space nil)

(defun remove-duplicates (n)
  "Helper function for counsel-dash backwards compatible remove-duplicates N."
  (cl-remove-duplicates n))

(defun new-shell ()
  "creates a shell with a given name (using eshell)"
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (eshell (concat "*" shell-name "*"))))

;;; custom_settings.el ends here
