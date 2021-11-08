;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq python-shell-interpreter "python3"
;; 	python-shell-interpreter-args "-i"
;; 	elpy-rpc-virtualenv-path 'current)
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

(use-package elpy
  :ensure t
  :defer t
  :bind
  (:map elpy-mode-map
        ("C-M-n" . elpy-nav-forward-block)
        ("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode)
         (elpy-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             '((elpy-company-backend :with company-yasnippet))))))
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-timeout 2))
