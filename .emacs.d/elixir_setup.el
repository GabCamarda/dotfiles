(require 'eglot)
(use-package elixir-mode :ensure t)

(add-hook
 'elixir-mode-hook
 (lambda ()
   (subword-mode)
   (eglot-ensure)
   (add-hook 'before-save-hook 'eglot-format nil t)))

;; Make sure to edit the path appropriately, use the .bat script instead for Windows
(add-to-list 'eglot-server-programs '(elixir-mode "~/.emacs.d/elixir-ls/language_server.sh"))
