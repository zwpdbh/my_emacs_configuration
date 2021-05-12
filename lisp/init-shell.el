;; prevent backspace from deleting my shell prompt
(setq comint-prompt-read-only t)

(when (maybe-require-package 'bash-completion)
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

(provide 'init-shell)