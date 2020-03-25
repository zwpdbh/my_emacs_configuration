;; - lsp should work with [[https://github.com/golang/tools/blob/master/gopls/README.md][gopls]]
;; - install it by ~go get golang.org/x/tools/gopls@latest~
;; - go-mode with ob-go

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(use-package go-mode
  :defer t
  :init 
  (if (string-equal system-type "gnu/linux")
      (add-to-list 'exec-path "/usr/local/go/bin")
    nil)
  :ensure t)

(use-package ob-go
  :defer 2 
  :ensure t
  :config
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t))))

(defun zw/lsp-go-steup ()
  (setq lsp-gopls-use-placeholders t)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook '(lambda ()
                           (lsp-mode t)
                           (lsp)
                           #'zw/lsp-go-steup))

(provide 'init-go)