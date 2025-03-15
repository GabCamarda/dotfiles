(use-package elixir-ts-mode :ensure t)
(use-package exunit
  :ensure t
  :config (setq transient-default-level 5))

(defun load-tags ()
  "Create tags programmatically when an Elixir buffer is opened"
  (interactive)
  (when (and (stringp buffer-file-name)
            (string= (file-name-extension buffer-file-name) "ex"))
    (let ((project-root (project-root (project-current))))
      (setq project-base-path (concat project-root "/TAGS"))
      (setq tags-add-tables t)
      (setq tags-revert-without-query t) ; don't query, revert silently
      ;; (setq tags-file-name "TAGS")
      ;; (tags-table-check-computed-list)
      (visit-tags-table project-base-path nil)
      (xref-etags-mode t)
      )))

(add-hook
 'elixir-ts-mode-hook
 (lambda ()
   (setq fill-column 120)
   (exunit-mode t)
   ;; (xref-etags-mode 1)
   ;; (add-to-list 'eglot-stay-out-of 'xref)
   ;; (run-with-idle-timer 1 t 'format-on-save)
   ))

(add-hook 'find-file #'load-tags)

;; Make sure to edit the path appropriately, use the .bat script instead for Windows
;; (add-to-list 'eglot-server-programs '(elixir-mode "~/.emacs.d/elixir-ls/language_server.sh"))
