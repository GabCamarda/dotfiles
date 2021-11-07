(setq user-mail-address "<gabrycamarda@gmail.com>"
      user-full-name "<Gabriele Camarda>")

(require 'nnir)
;; @see http://www.emacswiki.org/emacs/GnusGmail#toc1
;; (setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups
;; ask encryption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)
                                  (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")  ; Move expired messages to Gmail's trash.
                                  (nnmail-expiry-wait immediate) ; Mails marked as expired can be processed immediately.
 				  (nnir-search-engine imap)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

; NO 'passive
(setq gnus-use-cache t)

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(define-key gnus-group-mode-map
  ;; list all the subscribed groups even they contain zero un-read messages
  (kbd "o") 'my-gnus-group-list-subscribed-groups)
