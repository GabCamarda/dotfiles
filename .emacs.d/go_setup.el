;;; go_setup.el --- Setup emacs to use Go
(require 'use-package)
(use-package go-guru :ensure t)
(use-package flycheck-gometalinter :ensure t)
;; only run fast linters
(setq flycheck-gometalinter-fast t)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

;; Snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; based on the assumption that the following repos are installed and
;; available on exec-path.
;;
;; - github.com/nsf/gocode
;; - golang.org/x/tools/cmd/goimports
;; - github.com/rogpeppe/godef
;; - github.com/golang/lint

(use-package go-eldoc
  :requires go-mode
  :hook go-mode)

(use-package gotest
  :hook go-mode
  :requires go-mode)

;; maybe gopath isn't setup??
(use-package golint
  :requires go-mode
  :hook go-mode
  :init
  (add-to-list 'load-path (expand-file-name "~/src/golang.org/x/lint/misc/emacs"))

  :ensure-system-package
  ((golint . "go get -u golang.org/x/lint/golint")))

(use-package go-autocomplete
  :hook go-mode
  :requires go-mode
  :init
  ;; setting up autocomplete should happen after yasnippet so we don't duplciate tab bindings.
  (require 'auto-complete-config))

(use-package go-mode
  :commands go-mode
  :ensure-system-package
  ((goimports . "go get -u golang.org/x/tools/cmd/goimports")
   (godef . "go get -u github.com/rogpeppe/godef")
   (gocode . "go get -u github.com/nsf/gocode"))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v -cover && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-,") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "C-c C-c") 'compile)	  ; Invoke compiler
  (local-set-key (kbd "M-R") 'recompile)          ; Redo most recent compile cmd
)

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; go_setup.el ends here
