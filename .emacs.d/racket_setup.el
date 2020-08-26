(use-package racket-mode
  :ensure t)

;; Racket-mode settings
(defun my-racket-hook ()
  (setq racket-program "/usr/local/bin/racket")

  (racket-xp-mode t))
(add-hook 'racket-mode-hook #'my-racket-hook)
