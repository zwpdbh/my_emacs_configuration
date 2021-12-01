(when *win64*
  (when (maybe-require-package 'csharp-mode)
    (when (maybe-require-package 'omnisharp)
      ;; After the package is installed you’ll need to install the omnisharp server by running M-x omnisharp-install-server
      (defun my-csharp-mode-hook ()
        (omnisharp-mode t)
        (company-mode t)
        (setq-local company-backends (zw/add-to-company-backends 'company-omnisharp))
        (setq indent-tabs-mode nil)
        (setq c-syntactic-indentation t)
        (c-set-style "ellemtel")
        (setq c-basic-offset 4)
        (setq truncate-lines t)
        (setq tab-width 4)

        (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
        (local-set-key (kbd "C-c C-c") 'recompile)))
      (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)))

(provide 'init-csharp)