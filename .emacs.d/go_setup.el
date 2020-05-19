;;; go_setup.el --- Setup emacs to use Go

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (setq company-backends '(company-go))           ; set autocompletion with company-mode
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v -cover && go vet"))

  (local-set-key (kbd "C-c C-c c") 'compile)	  ; Invoke compiler
  (local-set-key (kbd "C-c C-c r") 'recompile)    ; Redo most recent compile cmd

  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
