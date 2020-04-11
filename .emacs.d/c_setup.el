;;; c_setup.el --- Setup emacs to use c/c++

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(defun irony-hook-fn()
  (setq irony-cdb-autosetup-compile-options t)
  (setq irony-eldoc t)
  )
(add-hook 'irony-mode-hook 'irony-hook-fn)

;;; c_setup.el ends here
