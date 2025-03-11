(use-package eglot :ensure t)
(use-package elixir-mode :ensure t)
;; (use-package elixir-mode :ensure t)
;; (use-package flycheck-credo :ensure t)
(use-package exunit :ensure t :config (setq transient-default-level 5))
(use-package
 emacs
 :ensure nil
 :custom

 ;; Should use:
 ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 ;; at least once per installation or while changing this list
 (treesit-language-source-alist
  '((heex "https://github.com/phoenixframework/tree-sitter-heex")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))

 ;; (major-mode-remap-alist
 ;;  '((elixir-mode . elixir-ts-mode)))
)

;; disable dialyzer globally
(setq-default eglot-workspace-configuration
	      '(:elixirLS (:dialyzerEnabled :json-false)))

;; (setq original-thing-at-point-file-name-chars thing-at-point-file-name-chars)
;; (setq thing-at-point-file-name-chars (concat thing-at-point-file-name-chars "!?"))

;; (eval-after-load 'flycheck
;;   '(flycheck-credo-setup))

(add-hook
 'elixir-mode-hook
 (lambda ()
   (setq fill-column 120)
   (setq-local devdocs-current-docs '("elixir~1.8"))
   (subword-mode)
   (eglot-ensure)
   (exunit-mode)
   (xref-etags-mode t)
   ;; disable go-to-implementation, etc as it never really works anyway
   ;; (const :tag "Documentation on hover" :hoverProvider)
   ;; (const :tag "Code completion" :completionProvider)
   ;; (const :tag "Function signature help" :signatureHelpProvider)
   ;; (const :tag "Go to definition" :definitionProvider)
   ;; (const :tag "Go to type definition" :typeDefinitionProvider)
   ;; (const :tag "Go to implementation" :implementationProvider)
   ;; (const :tag "Go to declaration" :declarationProvider)
   ;; (const :tag "Find references" :referencesProvider)
   ;; (const :tag "Highlight symbols automatically" :documentHighlightProvider)
   ;; (const :tag "List symbols in buffer" :documentSymbolProvider)
   ;; (const :tag "List symbols in workspace" :workspaceSymbolProvider)
   ;; (const :tag "Execute code actions" :codeActionProvider)
   ;; (const :tag "Code lens" :codeLensProvider)
   ;; (const :tag "Format buffer" :documentFormattingProvider)
   ;; (const :tag "Format portion of buffer" :documentRangeFormattingProvider)
   ;; (const :tag "On-type formatting" :documentOnTypeFormattingProvider)
   ;; (const :tag "Rename symbol" :renameProvider)
   ;; (const :tag "Highlight links in document" :documentLinkProvider)
   ;; (const :tag "Decorate color references" :colorProvider)
   ;; (const :tag "Fold regions of buffer" :foldingRangeProvider)
   ;; (const :tag "Execute custom commands" :executeCommandProvider)
   ;; (const :tag "Inlay hints" :inlayHintProvider)
   ;; (setq eglot-ignored-server-capabilities '(:definitionProvider, :typeDefinitionProvider, :implementationProvider, :declarationProvider))
   (add-to-list 'eglot-stay-out-of 'xref)
   ;; (run-with-idle-timer 1 t 'format-on-save)
   ))

(defun my-symbol-at-point ()
  "Prints symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
	(word (thing-at-point 'word)))
    (message symbol)
    (message word)))

(defun elixir-format ()
  "Call mix.format [file-path] on current buffer"
  (interactive)
  (when (and (stringp buffer-file-name)
	     (member (file-name-extension buffer-file-name) '("ex" "exs")))
    ;; (async-shell-command (format "mix format %s" buffer-file-name) buffer-file-name))
    (call-process-shell-command (format "mix format %s" buffer-file-name)))
  )

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

(defun create-tags ()
  "Create tags programmatically when an Elixir buffer is opened"
  (interactive)
  (when (and (stringp buffer-file-name)
	     (string= (file-name-extension buffer-file-name) "ex"))
    (call-process-shell-command "ctags -eR")
    (xref-etags-mode t)
    (let ((tags-revert-without-query t))  ; don't query, revert silently
      (setq tags-add-tables t)
      ;; (setq tags-file-name "TAGS")
      (tags-table-check-computed-list)
      (visit-tags-table "TAGS" nil)
      (create-tags-for-related-files)
      )))

(defun create-tags-for-related-files ()
  "Create tags for modules that are imported or aliased"
  (interactive)
  (let ((regex "Tiger\\(\\.\\w+\\)+")                      ; regex to find imported or aliased module in current buffer
	(project-root (or (project-root (project-current)) ; guess the project root with `project.el`
			  (error "No project found!"))))   ; otherwise throw an error
    (save-excursion
      (goto-char (point-min))                              ; start searching from the beginning of the buffer
      (while (re-search-forward regex nil t)               ; run regex defined above
	(let* ((match (downcase (match-string 0)))         ; get matches and downcase them
	       (relative-path (replace-regexp-in-string "\\." "/" match))
	       (full-path (expand-file-name (concat "lib/" relative-path) project-root)))
	  (setq directory-path (file-name-directory full-path))
	  (message "Found match: %s" match)
	  (message "Converted to path %s" directory-path)
	  (when (file-directory-p directory-path)
	    (let ((default-directory directory-path))
	      (call-process-shell-command "ctags -eR")
	      (visit-tags-table default-directory nil))))))))

(add-hook 'find-file-hook #'load-tags)
;; (add-hook 'after-save-hook #'format-on-save)

;; Make sure to edit the path appropriately, use the .bat script instead for Windows
(add-to-list 'eglot-server-programs '(elixir-mode "~/.emacs.d/elixir-ls/language_server.sh"))
