;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :defer t
  :mode "\\.s\\(cala\\|bt\\)$")

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

;; Compile scala file and run binary
(defun compile-scala ()
  "Compile this scala file and run its binary"
  (set (make-local-variable 'compile-command)
       (concat "scalac " buffer-file-name " && scala " buffer-file-name))

  (local-set-key (kbd "C-c C-c c") 'compile)
  (local-set-key (kbd "C-c C-c r") 'recompile)
  )

;; Evaluate region in Scala REPL
(defun eval-scala (start end)
  "Evaluate region in Scala REPL"
  (interactive (list (point) (mark)))
  ;; start scala if it hasn't started yet
  (unless (get-process "scala-repl")
    (let ((process-connection-type nil))  ; use a pipe
      (start-process "scala-repl" "*scala*"  "scala"))
    (set-buffer "*scala*")
    (special-mode)
    )
  ;; execute
  (process-send-region "scala-repl" start end)
  (process-send-string "scala-repl" "\n")
  ;;display buffer
  (display-buffer
   (get-buffer "*scala*")
   '((display-buffer-reuse-window
      display-buffer-pop-up-window
      display-buffer-pop-up-frame)
     (reusable-frames . 0)
     (window-height . 8) (window-width . nil)
   )
  )
)

(add-hook 'scala-mode-hook #'compile-scala)
