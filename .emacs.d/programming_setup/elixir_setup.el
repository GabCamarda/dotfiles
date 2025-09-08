(provide 'elixir_setup)

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

(defun load-elixir-tags (&optional path)
  "Create tags programmatically for Elixir mode"
  ;; (interactive "FElixir TAGS path (leave empty for project-root default): ")
  (interactive "P")
  (let ((tags-path (or path (project-root (project-current)))))
    (setq full-tags-path (concat tags-path "/TAGS"))
    (setq tags-add-tables t)
    (setq tags-revert-without-query t) ; don't query, revert silently
    (visit-tags-table full-tags-path nil)
    ))

(defun my/xref-find-definitions ()
  "try using etags first and fallback on dumb-jump otherwise"
  (interactive)
  (setq xref-backend-functions '(etags--xref-backend dumb-jump-xref-activate))
  (let* ((identifier (or (thing-at-point 'symbol t)
			 (xref--read-identifier "Find definitions of: ")))
	 (identifier (if (string-match-p "%" identifier)
			 (replace-regexp-in-string "%" "" identifier)
		       identifier))
	 (case-fold-search nil))
    (condition-case nil
	 (xref-find-definitions identifier)
      (user-error
       (setq xref-backend-functions '(dumb-jump-xref-activate))
       (xref-find-definitions identifier))))
  )

(defun flymake-elixir-command (filename)
  "Construct a command that flymake can use to check elixir source."
  (list "elixirc"
        "--ignore-module-conflict" ; needed to prevent from module redefinition warning.
        "+warn_obsolete_guard"
        "+warn_unused_import"
        "+warn_shadow_vars"
        "+warn_export_vars"
        "+strong_validation"
        "+report"
        filename))

(defun flymake-elixir-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (flymake-elixir-command (list local-file))))

(defun flymake-elixir-buffer-async (report-fn &rest _args)
  "Flymake backend to check only the current Elixir buffer using `elixirc`."
  (make-process
   :name "flymake-elixir"
   :buffer (generate-new-buffer " *flymake-elixir*")
   ;; :command (list "elixirc" "--warnings-as-errors" buffer-file-name)
   :command (list "elixirc"
		  "--ignore-module-conflict" ; needed to prevent from module redefinition warning.
		  "--warnings-as-errors"
		  buffer-file-name)
   :noquery t
   :file-handler t
   :sentinel
   (lambda (proc _event)
     (when (eq (process-status proc) 'exit)
       (with-current-buffer (process-buffer proc)
         (goto-char (point-min))
         (let (diags)
           (while (re-search-forward "^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" nil t)
           ;; (while (re-search-forward "^\\(** (.*) \\)?\\(.*\.ex\\):\\([0-9]+\\): \\(.*\\)$" nil t)
             (let ((file (match-string 1))
                   (line (string-to-number (match-string 2)))
                   (msg (match-string 3)))
               (push (flymake-make-diagnostic (current-buffer)
                                              (point-min) (point-max)
                                              :error msg)
                     diags)))
	   (message "%S" diags)
	   (message "%S" report-fn)
           (funcall report-fn diags)
	   ))
       (kill-buffer (process-buffer proc))))
   :plist `(:report-fn ,report-fn)
   )
  )

(defun flymake-elixir-buffer (report-fn &rest _args)
  "Simple Flymake backend for Elixir using `elixirc`."
  (let* ((source-buffer (current-buffer))
         (temp-file (make-temp-file "flymake-elixir" nil ".ex"))
         (command (list "elixirc" "--warnings-as-errors" temp-file))
         diagnostics)
    ;; Write the buffer contents to a temp file
    (with-temp-file temp-file
      (insert-buffer-substring source-buffer))
    ;; Run elixirc synchronously and capture output
    (with-temp-buffer
      (let ((exit-code (apply #'call-process (car command) nil t nil (cdr command))))
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\* (.*) \\(.+\\) found on \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\n\\s-*error: \\(.*\\)" nil t)
	  (message "%S" (match-string 2))
	  (message "%S" (match-string 3))
	  (message "%S" (match-string 4))
	  (message "%S" (match-string 5))
          (let* ((line (string-to-number (match-string 3)))
		 (column (string-to-number (match-string 4)))
                 (msg (match-string 5))
                 (beg (with-current-buffer source-buffer (line-beginning-position line)))
                 (end (with-current-buffer source-buffer (line-end-position line))))
            (push (flymake-make-diagnostic source-buffer beg end :error msg) diagnostics)))
        ;; Report collected diagnostics
	(message "%S" diagnostics)
        (funcall report-fn diagnostics)
        (delete-file temp-file)))))

(add-hook
 'elixir-ts-mode-hook
 (lambda ()
   (setq fill-column 120)

   (local-set-key (kbd "M-.") #'my/xref-find-definitions)
   (load-elixir-tags "/Users/gabrielecamarda/.asdf/installs/elixir/1.18.2-otp-27")
   (load-elixir-tags)
   (xref-etags-mode t)

   (use-package exunit :ensure t :defer t :init (exunit-mode))
   ;; (use-package flymake
   ;;   :ensure nil
   ;;   :config
   ;;   (setq flymake-no-changes-timeout nil
   ;; 	   flymake-start-on-save-buffer t
   ;; 	   flymake-start-syntax-check-on-newline nil
   ;; 	   flymake-show-diagnostics-at-end-of-line t)
   ;;   (add-hook 'flymake-diagnostic-functions #'flymake-elixir-buffer nil t)
   ;;   :init
   ;;   (flymake-mode t))
   ;; (add-to-list 'eglot-stay-out-of 'xref)
   ;; (run-with-idle-timer 1 t 'format-on-save)
   ))

(elixir-ts-mode)
