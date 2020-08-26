(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(beacon-color "#cc6666")
 '(company-begin-commands '(self-insert-command))
 '(company-dabbrev-code-ignore-case nil)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers nil)
 '(company-tooltip-align-annotations t)
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("679ee3b86b4b34661a68ba45bbd373eab0284caee6249139b2a090c9ddd35ce0" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(disable-outside-detected-project t)
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(global-company-mode t)
 '(ocamlformat-enable 'disable)
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("u" "Urgent things to do"
      ((tags-todo "URGENT"
		  ((org-agenda-overriding-header "Urgent things to do"))))
      nil nil)))
 '(package-selected-packages
   '(posframe php-mode yasnippet-snippets ws-butler wgrep use-package treepy sx sr-speedbar popup-imenu org-link-minor-mode neotree moe-theme magit lsp-ui lsp-java graphql google-c-style go-guru go-fill-struct go-errcheck go-eldoc go-autocomplete go-add-tags flycheck-gometalinter eglot dumb-jump dap-mode counsel-projectile counsel-dash company-lsp company-go color-theme-sanityinc-tomorrow change-inner autodisass-java-bytecode))
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(epa-file-enable)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background "red" :underline (:color "#373b41" :style wave)))))
 '(show-paren-match ((t (:background "color-233" :foreground "brightwhite")))))
