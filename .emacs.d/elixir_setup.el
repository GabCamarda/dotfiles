(require 'eglot)
(use-package elixir-mode :ensure t)
(use-package flycheck-credo :ensure t)
(use-package exunit :ensure t)

(eval-after-load 'flycheck
  '(flycheck-credo-setup))

(add-hook
 'elixir-mode-hook
 (lambda ()
   (subword-mode)
   (eglot-ensure)
   (flycheck-mode)
   (exunit-mode)
   ))

;; Make sure to edit the path appropriately, use the .bat script instead for Windows
(add-to-list 'eglot-server-programs '(elixir-mode "~/.emacs.d/elixir-ls/language_server.sh"))
