;; - lsp should work with [[https://github.com/golang/tools/blob/master/gopls/README.md][gopls]]
;; - install it by ~go get golang.org/x/tools/gopls@latest~
;; - go-mode with ob-go

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(when (maybe-require-package 'go-mode)
  (if (string-equal system-type "gnu/linux")
      (add-to-list 'exec-path "/usr/local/go/bin/")
    nil)
  (add-hook 'go-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook 'gofmt-before-save)
               ;; Godef, lets you quickly jump around the code
               ;; Install it by: go get github.com/rogpeppe/godef
               ;; Installing go-mode automatically installs godef bindings.
               (define-key go-mode-map (kbd "M-.") 'godef-jump)
               (define-key go-mode-map (kbd "M-,") 'pop-tag-mark))))

(when (maybe-require-package 'ob-go)
  (after-load 'org
    (add-to-list 'org-structure-template-alist '("go" . "src go"))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((go . t)))))

;; configure lsp related parameters
(defun zw/lsp-go-steup ()
  (setq lsp-gopls-use-placeholders t)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (zw/customize-lsp-ui-key-bindings))

;; Need to install gopls to use lsp
;; go get golang.org/x/tools/gopls@latest
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (add-hook 'go-mode-hook '(lambda ()
;;                           #'zw/lsp-go-steup))

(provide 'init-go)
