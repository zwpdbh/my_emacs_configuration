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
        (define-key csharp-mode-map (kbd "M-.") 'omnisharp-find-implementations)
        (if (fboundp 'zw/consult-ripgrep-at-point)
            (define-key csharp-mode-map (kbd "M-/") 'zw/consult-ripgrep-at-point)
          (define-key csharp-mode-map (kbd "M-/") 'zw/counsel-etags-grep-at-point))))
    
    (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

    (after-load 'org
      ;; Uncomment one of the following to be able to execute csharp code block.
      ;; (defun org-babel-execute:csharp (body params) body) ; Defun dummy function to execute.
      ;; (add-to-list 'zw/org-babel-load-language-list '(csharp . t)) ; Only enable this if you have ob-csharp
      (add-to-list 'org-structure-template-alist '("cs" . "src csharp"))
      (add-to-list 'org-structure-template-alist '("csharp" . "src csharp")))))

(provide 'init-csharp)