;; prevent backspace from deleting my shell prompt
(setq comint-prompt-read-only t)

(when (maybe-require-package 'bash-completion)
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

(add-hook 'shell-mode-hook
          #'(lambda ()
              (setq-local company-backends '((company-dabbrev company-files) company-keywords))
              (ansi-color-for-comint-mode-on)))

(add-hook 'sh-mode-hook
          #'(lambda ()
             (setq-local company-backends (zw/delete-from-company-backends 'company-capf))))

(provide 'init-shell)