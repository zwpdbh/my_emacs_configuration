(when (executable-find "dotnet")
  (when (maybe-require-package 'csharp-mode)
    (when (maybe-require-package 'omnisharp)
      ;; After the package is installed you’ll need to install the omnisharp server by running M-x omnisharp-install-server
      (defun my-csharp-mode-hook ()
        (omnisharp-mode t)
        (company-mode t)
        (flycheck-mode -1)
        (setq-local company-backends (zw/add-to-company-backends 'company-omnisharp))
        (setq indent-tabs-mode nil)
        (setq c-syntactic-indentation t)
        (c-set-style "ellemtel")
        (setq c-basic-offset 4)
        (setq truncate-lines t)
        (setq tab-width 4)

        (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
        (local-set-key (kbd "C-c C-c") 'recompile)
        (zw/counsel-etags-setup)))
    (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

    (after-load 'org
      ;; Defun dummy execution
      ;; (defun org-babel-execute:csharp (body params) body)
      (add-to-list 'zw/org-babel-load-language-list '(csharp . t))
      (add-to-list 'org-structure-template-alist '("cs" . "src csharp"))
      (add-to-list 'org-structure-template-alist '("csharp" . "src csharp")))))

(provide 'init-csharp)