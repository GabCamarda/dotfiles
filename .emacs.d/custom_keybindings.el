;;; custom_keybinding.el --- Provides useful custom keybindings

;; annoyances with UK keyboard layout
(define-key key-translation-map (kbd "M-3") (kbd "#"))

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)

(global-set-key (kbd "M-p") 'beginning-of-defun)
(global-set-key (kbd "M-n") 'end-of-defun)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key (kbd "C-<backspace>") 'contextual-backspace)

;; Org-mode, the following lines are always needed.
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

;; allow switching windows easier
(global-set-key (kbd "M-0") 'other-window)
(global-set-key (kbd "M-9") 'prev-window)

;; Programming specific bindings
(define-key prog-mode-map (kbd "C-c f n e") 'flymake-goto-next-error)
(define-key prog-mode-map (kbd "C-c f p e") 'flymake-goto-prev-error)

(defun prev-window ()
  (interactive)
  (other-window -1))

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

;; aliases
(defalias 'qrr 'query-replace-regexp)
