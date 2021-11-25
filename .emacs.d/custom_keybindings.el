;;; custom_keybinding.el --- Provides useful custom keybindings

(require 'use-package)
(use-package change-inner :ensure t)     ; Similar to ci or co in Vim
(use-package ibuffer :ensure t)
(use-package recentf :init)
(use-package swiper :ensure t)
(use-package counsel :ensure t)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "M-x") 'counsel-M-x)
;; rebind default old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; File finding
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c C-s a") 'swiper-all)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-f") 'counsel-ag)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c n d") 'find-name-dired)

;; get rid of `find-file-read-only' and replace it with something
;; more useful
(recentf-mode t)
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)
;; Tell Ivy to not autocomplete
(define-key minibuffer-local-map (kbd "M-RET") 'ivy-immediate-done)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x w j") 'webjump)
(global-set-key (kbd "C-x w .") 'browse-url-at-point)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)

;; Clever newlines
(global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-dwim)

;; Allow using Ctrl for invoking M-x
(global-set-key (kbd "C-x C-m") 'counsel-M-x)
(global-set-key (kbd "C-c C-m") 'counsel-M-x)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "M-p") 'beginning-of-defun)
(global-set-key (kbd "M-n") 'end-of-defun)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)
(global-set-key (kbd "C-x C-n") #'eos/narrow-or-widen-dwim)

;; Org-mode, the following lines are always needed.
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-x p e") 'my/project-eshell)
(global-set-key (kbd "C-x p s") 'my/project-shell)

;; allow switching windows easier
(global-set-key (kbd "M-0") 'other-window)
(global-set-key (kbd "M-9") 'prev-window)
(defun prev-window ()
  (interactive)
  (other-window -1))

(defun my/project-eshell ()
  (interactive)
  (let ((buf (project-eshell)))
	(switch-to-buffer (other-buffer buf))
	(switch-to-buffer-other-window buf)
	))

(defun my/project-shell ()
  (interactive)
  (let ((buf (project-shell)))
	(switch-to-buffer (other-buffer buf))
	(switch-to-buffer-other-window buf)
  ))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
		      (buffer-substring (region-beginning) (region-end))
		    (prog1 (thing-at-point 'line)
		      (end-of-line)
		      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
			  (newline))))))
	(dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
	  (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
	(if (> 0 n)                             ;Comment out original with negative arg
	    (comment-region (line-beginning-position) (line-end-position)))
	(forward-line 1)
	(forward-char pos)))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode)
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

(defun eos/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; aliases
(defalias 'qrr 'query-replace-regexp)
;;; custom_keybindings.el ends here
