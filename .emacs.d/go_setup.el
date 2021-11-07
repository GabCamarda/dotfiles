;;; go_setup.el --- Setup emacs to use Go

(defun eglot-format-buffer-on-save ()
  (if (string-match "command not found"
		    (with-temp-buffer (shell-command "gopls" t)
				      (buffer-string)))
      (message "gopls not found. Installing latest version for LSP.")
      (shell-command "go get golang.org/x/tools/gopls@latest" t))   ; download gopls package if not found

  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v -cover && go vet"))

  (local-set-key (kbd "C-c C-c c") 'compile)	  ; Invoke compiler
  (local-set-key (kbd "C-c C-c r") 'recompile)    ; Redo most recent compile cmd
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(setq-default eglot-workspace-configuration
              '((:gopls . (:usePlaceholders t))))

(add-hook 'project-find-functions #'project-find-go-module)
(add-hook 'go-mode-hook #'yas-minor-mode)
(add-hook 'go-mode-hook #'eglot-ensure)
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
