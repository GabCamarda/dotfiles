;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(use-package ocamlformat :ensure t)

(use-package utop :ensure t)
;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")

(add-hook 'tuareg-mode-hook (lambda ()
			      (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
			      (define-key tuareg-mode-map (kbd "M-.") #'merlin-locate)
			      (define-key tuareg-mode-map (kbd "M-,") #'merlin-pop-stack)
			      (add-hook 'before-save-hook #'ocamlformat-before-save)))
