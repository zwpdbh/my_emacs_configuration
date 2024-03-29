(when (executable-find "dotnet")
  (maybe-require-package 'csharp-mode)

  (defun my-csharp-mode-hook ()
    (interactive)
    (setq-local c-basic-offset 4)
    (setq-local indent-tabs-mode nil)
    (setq-local c-syntactic-indentation t)
    (setq-local truncate-lines t)
    (setq-local tab-width 4)
    
    (flycheck-mode -1)
    (local-set-key (kbd "C-c C-c") 'recompile)
    (zw/counsel-etags-setup))

  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
  (after-load 'org
    ;; Uncomment one of the following to be able to execute csharp code block.
    ;; (defun org-babel-execute:csharp (body params) body) ; Defun dummy function to execute.
    ;; (add-to-list 'zw/org-babel-load-language-list '(csharp . t)) ; Only enable this if you have ob-csharp
    (add-to-list 'org-structure-template-alist '("cs" . "src csharp"))
    (add-to-list 'org-structure-template-alist '("csharp" . "src csharp"))))


;; Derive our custom csweb-mode from web-mode to do customization
(define-derived-mode csweb-mode web-mode "csharpweb-mode")
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . csweb-mode))
(add-hook 'csweb-mode-hook
          #'(lambda ()
             (setq-local tab-width 4)
             (setq-local web-mode-markup-indent-offset 4)
             (setq-local web-mode-css-indent-offset 4)
             (setq-local web-mode-code-indent-offset 4)))

(provide 'init-csharp)
