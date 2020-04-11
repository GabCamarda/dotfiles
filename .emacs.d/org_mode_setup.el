;;; org_mode_setup --- Customise org mode
;; tags
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "IN-PROGRESS(p/!)" "WAITING(w@/!)"
		  "IN-REVIEW(r/!)" "BLOCKED(b@/!)" "|"
		  "MERGED(m/!)" "DONE(d)" "CANCELED(c@/!)")))

;; todo fast selection via C-c C-t [key]
(setq org-use-fast-todo-selection t)
;; change todo status without applying todo state events (e.g. record timestamp)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; org files directory
(setq org-agenda-files '("~/Documents/org/"))

;; refile options
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; log notes in :LOGBOOK: drawer
(setq org-log-into-drawer t)
;; include archives in searches
(setq org-agenda-text-search-extra-files '(agenda-archives))
;; spacing between headings
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
;; all children todo must be set to done before the parent can be done
(setq org-enforce-todo-dependencies t)
;; add a timestamp log when todo is marked to done
(setq org-log-done (quote time))
;; add a timestamp log when todo deadlines is rescheduled
(setq org-log-redeadline (quote time))
;; add a timestamp log when todo is rescheduled
(setq org-log-reschedule (quote time))

;; capture templates
(setq org-capture-templates
  '(
    ("m" "TODO from Mail" entry (file+headline "~/Documents/org/todo.org" "Inbox")
     "* TODO %?, Link: %a")
    ("t" "TODO format" entry (file "~/Documents/org/todo.org") (file "~/.org/todo-template.txt"))
   )
  )

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;;; org_mode_setup.el ends here
