(use-package erc
  :preface
  (defun my/erc-browse-last-url ()
    "Searchs backwards through an ERC buffer, looking for a URL. When a URL is
     found, it prompts you to open it."
    (interactive)
    (save-excursion
      (let ((ffap-url-regexp "\\(https?://\\)."))
        (ffap-next-url t t))))

  (defun my/erc-count-users (server)
    "Displays the number of users and ops connected on the current channel."
    (interactive "sServer")
    (if (get-buffer (concat server ":6667"))
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let ((hash-table (with-current-buffer (erc-server-buffer)
                                  erc-server-users))
                    (users 0)
                    (ops 0))
                (maphash (lambda (k v)
                           (when (member (current-buffer)
                                         (erc-server-user-buffers v))
                             (cl-incf users))
                           (when (erc-channel-user-op-p k)
                             (cl-incf ops)))
                         hash-table)
                (message "%d users (%s ops) are online on %s" users ops channel))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun my/erc-get-ops ()
    "Displays the names of ops users on the current channel."
    (interactive "sServer")
    (if (get-buffer (concat server ":6667"))
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let (ops)
                (maphash (lambda (nick cdata)
                           (if (and (cdr cdata)
                                    (erc-channel-user-op (cdr cdata)))
                               (setq ops (cons nick ops))))
                         erc-channel-users)
                (if ops
                    (message "The online ops users are: %s"  (mapconcat 'identity ops " "))
                  (message "There are no ops users online on %s" channel)))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun my/erc-notify (nickname message)
    "Displays a notification message for ERC."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str (string-trim (replace-regexp-in-string "\n+" " " str))))

  (defun my/erc-reset-track-mode ()
    "Resets ERC track mode."
    (interactive)
    (setq erc-modified-channels-alist nil)
    (erc-modified-channels-update)
    (erc-modified-channels-display)
    (force-mode-line-update))

  (defun my/erc-start-or-switch (server)
    "Connects to ERC, or switch to last active buffer."
    (interactive "sServer")
    (if (get-buffer (concat server ":6667"))
        (erc-track-switch-buffer 1)
      (erc :server server :port 6667 :nick "_bushido_")))

  (defvar my/erc-channels-to-visit nil
    "Channels that have not yet been visited by erc-next-channel-buffer")
  (defun my/erc-next-channel-buffer ()
    "Switch to the next unvisited channel. See erc-channels-to-visit"
    (interactive)
    (when (null erc-channels-to-visit)
      (setq erc-channels-to-visit
	    (remove (current-buffer) (erc-channel-list nil))))
    (let ((target (pop erc-channels-to-visit)))
      (if target
	  (switch-to-buffer target))))

  (defun my/erc-joined-channels ()
    "Return all the channels you're in as a list.  This does not include queries."
    (save-excursion
      ;; need to get out of ERC mode so we can have *all* channels returned
      (set-buffer "*scratch*")
      (mapcar #'(lambda (chanbuf)
                  (with-current-buffer chanbuf (erc-default-target)))
              (erc-channel-list erc-process))))

  :hook ((ercn-notify . my/erc-notify)
         (erc-send-pre . my/erc-preprocess))

  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#ocaml" "#bash" "#go-nuts" "#haskell"
                                  "#emacs" "#latex" "#org-mode")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-header-line-format "%n on %t (%m)")
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-join-buffer 'bury)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  ;; (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules)
  )

(use-package erc-hl-nicks :after erc)
(use-package erc-image :after erc)

(defun bitlbee-netrc-identify ()
  "Auto-identify for Bitlbee channels using authinfo or netrc.

    The entries that we look for in netrc or authinfo files have
    their 'port' set to 'bitlbee', their 'login' or 'user' set to
    the current nickname and 'server' set to the current IRC
    server's name.  A sample value that works for authenticating
    as user 'keramida' on server 'localhost' is:

    machine localhost port bitlbee login <email> password <supersecret>"
  (interactive)
  (when (string= (buffer-name) "&bitlbee")
    (let* ((secret (plist-get (nth 0 (auth-source-search :max 1
							 :host erc-server
							 :user (erc-current-nick)
							 :port "bitlbee"))
			      :secret))
	   (password (if (functionp secret)
			 (funcall secret)
		       secret)))
      (erc-message "PRIVMSG" (concat (erc-default-target) " " "identify" " " password) nil))))

;; Enable the netrc authentication function for &biblbee channels.
(add-hook 'erc-join-hook 'bitlbee-netrc-identify)
