(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-block-padding 2))

(use-package web-mode
  :ensure t
  :mode (("\\.php$" .  web-mode)
         ("\\.html$" .  web-mode)
	 ("\\.css$" .  web-mode)
	 ("\\.js$" .  web-mode)
	 ("\\.jsx$" .  web-mode)
	 )
  :hook (web-mode . web-mode-init-hook)
  :config (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  )

;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
